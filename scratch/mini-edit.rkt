#lang racket

;; SCRATCH -- a little grid editor over the segmenting split, with a small vim
;; vocabulary (counts compose: 3l, 2j, 5x, 2dd):
;;   [n]h / [n]l   left / right (char-wise, crosses line ends)
;;   [n]k / [n]j   up / down (column kept as a GOAL: short lines clamp, never reset)
;;   linecol [r c] (alias: g) enter line/column (grid) mode, optionally AT
;;                 row r column c;
;;   sexp [i ...]  (alias: s) enter sexp mode, optionally AT a spine address,
;;                 written OUTERMOST slot first -- the parent before the child,
;;                 the REVERSE of the status line's innermost-first spines.
;;                 Either keyword without an address stays at the cursor, and
;;                 either with one doubles as a goto within its mode
;;   0 / $         line start / line end
;;   [n]x          delete AT the cursor (vim x) -- front-anchored, cursor holds;
;;                 in SEXP mode, x and X delete the WHOLE form the cursor is on
;;                 ([n] forms), through the region machinery
;;   r <txt>       replace at the cursor, by the same dispatch as x: the
;;                 SELECTION when one is active, every highlighted run when
;;                 /<word> is on, the form the cursor is on in sexp mode (its
;;                 trailing whitespace kept), else the single char (vim r)
;;   [n]b / [n]X   backspace                    -- back-anchored via the flip
;;   [n]dd         delete whole lines
;;   u             undo the last edit (unbounded history; moves are not edits)
;;   /<word>       highlight every occurrence of <word> (occur-guide in the
;;                 render product), persisting across edits; / alone clears.
;;                 The search is itself a kind of CURSOR: while it is on, x
;;                 deletes and r <txt> substitutes every highlighted run (a
;;                 run = a maximal welded stretch of occurrences, so adjacent
;;                 or overlapping matches replace as one)
;;   i <txt>       insert at the cursor (\n in txt inserts a newline)
;;   v / V         visual selection, charwise / linewise: h/l/j/k/0/$ extend it,
;;                 d (or x) deletes it, v/V again cancels; v inside SEXP mode
;;                 selects structurally instead -- the anchor and the endpoint
;;                 are sexp cuts, hjkl move the endpoint by spine (sibs / out /
;;                 in). Unlike vim, v selections are EXACTLY the region between
;;                 the two cuts -- empty at birth (an empty focus), no
;;                 char-under-the-cursor inclusivity; V stays line-inclusive
;;                 (its coordinates are line segments, not cuts)
;;   q             quit
;; Edits left of the cursor go through the flip machinery: cut at the FRONT grid
;; guide, transform the before pieces, reassemble, re-find the cursor with the
;; BACK-anchored guide (flip-linecol) -- no cursor arithmetic. Edits right of the
;; cursor (x) need no flip at all: the front name is already stable there. Run
;; with -i for interactive mode.
;;
;; ORGANISATION: a mode is CHURCH-ENCODED -- not a record but its own
;; eliminator, a function handing its edit stuff to a consumer:
;;     mode : (enter h l j k target keep -> A) -> A
;; The loop's cursor state IS the current mode's guide. enter : doc guide ->
;; guide is the only cross-mode operation, and it never opens the incoming
;; guide -- it uses it to SPLIT the text and reads the mode's address off the
;; summaries flanking the gap (a flip into the mode's coordinate system), so
;; any mode enters from any cursor. hjkl are guide -> guide functions
;; rewriting the address in the spec (which only the minting mode reads), and
;; the x/r target is guide -> seg-guide, a 3-tag before/focus/after region
;; factor selecting the area the edit acts on; x deletes the focus, r
;; substitutes it. The selection (vis-region, its anchor a stored guide) and
;; the search cursor (occur-guide) produce seg-guides of the same shape, so
;; every deleting / replacing edit funnels through the one subst-focus.

(require racket/match racket/string racket/list
         "segment.rkt"
         "render.rkt"
         "../rope-core.rkt"
         "../summaries/summaries.rkt"
         "../summaries/lisp-summary.rkt"
         "../summaries/occur-summary.rkt")

(provide mini-edit)

(define buf (bundle char-smr linecol-smr lisp-smr atomhash-smr))
(define (before? s) (eq? (last (car s)) 'before))

(define (cut-at text g [gsmr linecol-smr])       ; -> (values pre post) piece lists
  (define segs ((segment (guide-product line-guide lisp-run-guide (guide->sides gsmr g)))
                ((make-rope buf) text)))
  (splitf-at segs before?))

(define (pos-of pre)                             ; (row . col) of the cut, off the summaries
  (if (null? pre)
      (cons 0 0)
      (let ([v (apply linecol-smr (map cdr pre))])
        (cons (linecol-lines v) (linecol-cols v)))))

(define (pos->offset text row col)
  (define-values (pre _) (cut-at text (grid-at row col)))
  (for/sum ([p (in-list pre)]) (char-smr (cdr p))))

(define (offset->pos text n)
  (define v (linecol-smr (substring text 0 n)))
  (cons (linecol-lines v) (linecol-cols v)))

(define (line-count text) (add1 (linecol-lines (linecol-smr text))))

;; one edit through the flip: xform : before-piece list -> piece list
(define (edit-through text row col xform)
  (define doc (( make-rope buf) text))
  (define gb  (flip-linecol (grid-at row col) doc))     ; back name, BEFORE the edit
  (define-values (pre post) (cut-at text (grid-at row col)))
  (define text1 (format "~a" (apply (make-rope buf)
                                    (append (xform (map cdr pre)) (map cdr post)))))
  (define-values (pre1 _) (cut-at text1 gb))            ; the flipped guide re-finds it
  (define p (pos-of pre1))
  (values text1 (car p) (cdr p)))

(define ((insert-op str) ps) (append ps (list str)))
(define ((trim-op n) ps)                         ; drop n chars from the end of the pieces
  (define t  (string-append* (map (lambda (p) (format "~a" p)) ps)))
  (define t* (substring t 0 (max 0 (- (string-length t) n))))
  (if (string=? t* "") '() (list t*)))

(define (line-len text row)
  (string-length (list-ref (string-split text "\n" #:trim? #f) row)))

(define (land text g)                            ; land ANY callable guide: cut the text
  (define-values (pre _) (cut-at text g buf))    ;   with it, read (row . col) off the
  (pos-of pre))                                  ;   left part -- the display view

;; ---------- sexp mode: edits over the refined index ----------
;; Mid-atom positions are first-class sexp targets, as (spine . offset); edits
;; cut with the refined front guide and re-find the cursor with flip-sexp, the
;; structure-true back anchor.
(define (edit-through/sexp text ix k xform)      ; the sexp-anchored edit
  (define doc ((make-rope buf) text))
  (define gb  (flip-sexp (lisp-slot-guide+ ix k) doc))
  (define-values (pre post) (cut-at text (lisp-slot-guide+ ix k) buf))
  (define text1 (format "~a" (apply (make-rope buf)
                                    (append (xform (map cdr pre)) (map cdr post)))))
  (define-values (pre1 _) (cut-at text1 gb buf))
  (define p (pos-of pre1))
  (values text1 (car p) (cdr p)))

(define (spine-move ix op n)                     ; h/l: siblings; k: out; j: in
  (case op
    [(h) (cons (max 0 (- (car ix) n)) (cdr ix))]
    [(l) (cons (+ (car ix) n) (cdr ix))]
    [(k) (let up ([ix ix] [n n])
           (if (or (zero? n) (null? (cdr ix))) ix (up (cdr ix) (sub1 n))))]
    [(j) (let down ([ix ix] [n n])
           (if (zero? n) ix (down (cons 0 ix) (sub1 n))))]))

;; ---------- modes, church-encoded, over spec'd cursor guides ----------
;; A mode organises its commands around the CURSOR AS A GUIDE (a guide/s whose
;; spec is its address, so it is both callable and readable). Its edit stuff --
;; the four movements and the selection -- comes as a church tuple: the mode
;; applies a consumer to, in order,
;;   enter  : doc guide -> guide             the mode's cursor at ANY guide's cut:
;;                                           split the text with it and read the
;;                                           address off the summaries at the gap
;;                                           (never opens the incoming guide)
;;   h l j k: doc n -> guide -> guide        movements REWRITE the guide's spec
;;   target : doc n guide -> seg-guide       the area x/r act on, as a 3-tag
;;                                           before/focus/after region factor
;;   keep   : focus-text -> string           what r appends after the replacement
;; and the consumer binds them under its own names -- no struct, no accessors.

(define (grid-mode use)
  (use
   (lambda (doc g)                             ; enter: the left part's linecol
     (define p (land doc g))
     (grid-at/s (car p) (cdr p)))
   (lambda (doc n) (lambda (g)                 ; h/l: char-wise, crossing line ends
                     (match (spec-of g)
                       [(list 'grid-at r c)
                        (define p (offset->pos doc (max 0 (- (pos->offset doc r c) n))))
                        (grid-at/s (car p) (cdr p))])))
   (lambda (doc n) (lambda (g)
                     (match (spec-of g)
                       [(list 'grid-at r c)
                        (define p (offset->pos doc (min (string-length doc)
                                                        (+ (pos->offset doc r c) n))))
                        (grid-at/s (car p) (cdr p))])))
   (lambda (doc n) (lambda (g)                 ; j/k: rows, the column kept
                     (match (spec-of g)
                       [(list 'grid-at r c)
                        (grid-at/s (min (sub1 (line-count doc)) (+ r n)) c)])))
   (lambda (doc n) (lambda (g)
                     (match (spec-of g)
                       [(list 'grid-at r c) (grid-at/s (max 0 (- r n)) c)])))
   (lambda (doc n g)                           ; target: the n chars right of the cursor
     (match (spec-of g)
       [(list 'grid-at r c)
        (define o (pos->offset doc r c))
        (guides->region buf g (char-at (min (string-length doc) (+ o n))))]))
   (lambda (T) "")))                           ; replacement is exact

(define (sexp-mode use)
  (use
   (lambda (doc g)                             ; enter: read the gap's summaries as a
     (define-values (pre post) (cut-at doc g buf))      ; refined index (nav-index:
     (define-values (ix k)                              ; arm exit, head class, tlen)
       (nav-index (apply buf (map cdr pre)) (apply buf (map cdr post))))
     (lisp-slot-guide/s ix k))
   (lambda (doc n) (lambda (g)                 ; hjkl: pure spine arithmetic --
                     (match (spec-of g)        ;   guide -> guide, no doc consulted
                       [(list 'lisp-slot ix _) (lisp-slot-guide/s (spine-move ix 'h n))])))
   (lambda (doc n) (lambda (g)
                     (match (spec-of g)
                       [(list 'lisp-slot ix _) (lisp-slot-guide/s (spine-move ix 'l n))])))
   (lambda (doc n) (lambda (g)
                     (match (spec-of g)
                       [(list 'lisp-slot ix _) (lisp-slot-guide/s (spine-move ix 'j n))])))
   (lambda (doc n) (lambda (g)
                     (match (spec-of g)
                       [(list 'lisp-slot ix _) (lisp-slot-guide/s (spine-move ix 'k n))])))
   (lambda (doc n g)                           ; target: the n whole forms at the cursor
     (match (spec-of g)
       [(list 'lisp-slot ix _)
        (define h (floor (car ix)))
        (guides->region buf (lisp-slot-guide (cons h       (cdr ix)))
                            (lisp-slot-guide (cons (+ h n) (cdr ix))))]))
   (lambda (T) (car (regexp-match #px"\\s*$" T)))))   ; keep the form's trailing ws

;; ---------- visual selection: the region factor over guides->region ----------
;; A selection is a REGION between two cuts -- the zipper's focus as tags.
;; vis = (list kind anchor-guide), kind 'line | 'char | 'sexp: the anchor is
;; the cursor GUIDE at v/V time, the other end is the current cursor guide;
;; both the highlight (render's #:region) and the deletion run through the
;; same guides->region factor: delete = segment, drop the 'focus pieces,
;; reassemble.
(define (cut-offset text g)                      ; land a classic guide: chars left of it
  (define-values (pre _) (cut-at text g buf))
  (for/sum ([p (in-list pre)]) (char-smr (cdr p))))

(define (vis-region text vis g)                  ; -> (cons gl gr)
  (match vis
    [(list 'line ga)
     (let* ([ra (car (land text ga))] [rc (car (land text g))]
            [r1 (min ra rc)]          [r2 (max ra rc)])
       ;; rows r1..r2 inclusive, vim dd semantics: the right cut is the next
       ;; row's start (taking r2's newline); deleting through the END of the
       ;; text takes the PRECEDING newline instead
       (if (and (= r2 (sub1 (line-count text))) (> r1 0))
           (cons (line-end-at (sub1 r1)) (char-at (string-length text)))
           (cons (grid-at r1 0) (grid-at (add1 r2) 0))))]
    [(list 'char ga)
     (let ([oa (cut-offset text ga)]             ; exactly the chars between the
           [oc (cut-offset text g)])             ; two cuts -- coincident = empty
       (cons (char-at (min oa oc)) (char-at (max oa oc))))]
    [(list 'sexp ga)
     ;; both ends are already cut guides, ordered by landing; the selection is
     ;; exactly the region between them -- an anchor just placed is an EMPTY
     ;; focus, and l then takes in the next whole form (cut to cut)
     (if (<= (cut-offset text ga) (cut-offset text g))
         (cons ga g)
         (cons g ga))]))

(define (subst-focus text factor f)              ; factor: a 3-tag seg-guide;
  (define segs ((segment (guide-product line-guide lisp-run-guide factor))
                ((make-rope buf) text)))        ; f : focus text -> replacement
  (define (side-of s) (last (car s)))
  (define (pieces side) (for/list ([s (in-list segs)] #:when (eq? (side-of s) side)) (cdr s)))
  (define focus (string-append* (map (lambda (p) (format "~a" p)) (pieces 'focus))))
  (define o1 (for/sum ([p (in-list (pieces 'before))]) (char-smr p)))
  (define t  (format "~a" (apply (make-rope buf)
                                 (append (pieces 'before) (list (f focus)) (pieces 'after)))))
  (define p  (offset->pos t (min o1 (string-length t))))
  (values t (car p) (cdr p)))                    ; -> (values text1 row col), cursor at
                                                 ;    the focus's start
(define (region-subst text region f)
  (subst-focus text (guides->region buf (car region) (cdr region)) f))

(define (region-delete text region) (region-subst text region (lambda (_) "")))

;; the search as a cursor: with /<word> on, x and r act on the OCCUR pieces --
;; segment by the occur guide alone, substitute every 'occur run
(define occur-buf (bundle char-smr occur-smr))
(define (occur-edit text W repl)
  (define segs ((segment (occur-guide W)) ((make-rope occur-buf) text)))
  (format "~a" (apply (make-rope occur-buf)
                      (for/list ([s (in-list segs)])
                        (if (eq? (car s) 'occur) repl (cdr s))))))

(define (delete-lines text row n)                ; [n]dd
  (define ls   (string-split text "\n" #:trim? #f))
  (define keep (append (take ls (min row (length ls)))
                       (drop ls (min (+ row n) (length ls)))))
  (values (string-join keep "\n") (max 0 (min row (sub1 (max 1 (length keep)))))))

(define (mini-edit [text0 "(define (f x)\n  (g \"ab\" x))\n(h 1)"] #:script [script0 #f])
  (let loop ([text text0] [g (grid-at/s 0 0)] [script script0] [vis #f]
             [hist '()] [hl #f])
    (define (hist+ t)                            ; push the pre-edit state, no-op edits skipped
      (if (equal? t text) hist (cons (list text g) hist)))
    (define-values (six sk)                      ; the mode lives in the cursor guide's spec
      (match (spec-of g)
        [(list 'lisp-slot ix k) (values ix k)]
        [_                      (values #f 0)]))
    (define M (if six sexp-mode grid-mode))
    (define-values (row col)                     ; the display view: grid reads its spec
      (if six                                    ;   raw (the goal column), sexp lands
          (let ([p (land text g)]) (values (car p) (cdr p)))
          (match (spec-of g) [(list 'grid-at r c) (values r c)])))
    (define (reenter t r c)                      ; after an edit: re-enter the mode at the cut
      (M (lambda (enter . _) (enter t (grid-at/s r c)))))
    (printf "~n")
    (if vis
        (render-viewport text 0 99 #:region (vis-region text vis g) #:hl hl)
        (render-viewport text 0 99 g #:hl hl))
    (match vis
      [(list 'sexp ga)
       (match-define (list 'lisp-slot aix ak) (spec-of ga))
       (printf "[VISUAL SEXP ~a+~a..~a+~a]  h/l sibs  k out  j in  d delete  v cancel  q~n> "
               aix ak six sk)]
      [(list kind ga)
       (define ap (land text ga))
       (printf "[VISUAL~a ~a:~a..~a:~a]  h/l/j/k/0/$ extend  d delete  ~a cancel  q~n> "
               (if (eq? kind 'line) " LINE" "") (car ap) (cdr ap) row col
               (if (eq? kind 'line) "V" "v"))]
      [#f
       (define hl* (if hl (format "  /~a" hl) ""))
       (if six
           (printf "[SEXP ~a:~a ~a+~a~a]  h/l sibs  k out  j in  v select  g grid  (edits as usual)  q~n> "
                   row col six sk hl*)
           (printf "[~a:~a~a]  h/l  k/j  0/$  s sexp  v/V select  /<w> hl  i <text>  x del  b bksp  dd  u undo  q~n> "
                   row col hl*))])
    (define cmd
      (cond [(not script)                       ; 'any: eat \r\n whole -- a Windows console
             (let ([l (read-line (current-input-port) 'any)])   ; line otherwise arrives "j\r"
               (if (eof-object? l) "q" (string-trim l "\r" #:left? #f)))]
            [(null? script) "q"]
            [else (car script)]))
    (define script* (if (and script (pair? script)) (cdr script) script))
    (when script (printf "~a~n" cmd))
    (match cmd
      ["q" (printf "bye~n")]
      [(regexp #rx"^(linecol|g)( +([0-9]+) +([0-9]+))? *$" (list _ _ addr r* c*))
       (define g* (grid-mode                     ; address: a grid cut to enter AT --
                   (lambda (enter . _)           ;   enter's landing does the clamping;
                     (enter text                 ;   no address: enter at the cursor
                            (if addr
                                (grid-at/s (min (string->number r*) (sub1 (line-count text)))
                                           (string->number c*))
                                g)))))
       (loop text g* script* #f hist hl)]
      [(regexp #rx"^(sexp|s)(( +[0-9]+)+)? *$" (list _ _ addr _))
       (define g* (if addr                       ; address: a spine, OUTERMOST slot first
                      (lisp-slot-guide/s         ;   (reversed into the internal spine)
                       (reverse (map string->number (string-split addr))))
                      (sexp-mode (lambda (enter . _) (enter text g)))))
       (loop text g* script* #f hist hl)]
      ["u" (match hist                           ; undo: pop the last edit's pre-state
             [(cons (list t g0) rest) (loop t g0 script* #f rest hl)]
             ['() (loop text g script* vis hist hl)])]
      [(regexp #rx"^/(.*)$" (list _ w))          ; highlight <word>; / alone clears
       (loop text g script* vis hist (if (string=? w "") #f w))]
      ["v" (loop text g script*                  ; visual: structural in sexp mode,
             (match vis                          ; charwise in grid; again cancels;
               [#f (list (if six 'sexp 'char) g)]
               [(list 'line ga) (list 'char ga)] ; v in linewise: switch, keep anchor
               [_ #f])
             hist hl)]
      ["V" #:when (not six)                      ; linewise visual (grid mode only)
       (loop text g script*
             (match vis
               [#f (list 'line g)]
               [(list 'char ga) (list 'line ga)] ; V in charwise: switch, keep anchor
               [_ #f])
             hist hl)]
      ["0" (loop text (M (lambda (enter . _) (enter text (grid-at/s row 0))))
                 script* vis hist hl)]
      ["$" (loop text (M (lambda (enter . _) (enter text (grid-at/s row (line-len text row)))))
                 script* vis hist hl)]
      [(regexp #rx"^i (.*)$" (list _ s))
       (define str (string-replace s "\\n" "\n"))
       (define-values (t r c)
         (if six
             (edit-through/sexp text six sk (insert-op str))
             (edit-through text row col (insert-op str))))
       (loop t (reenter t r c) script* #f (hist+ t) hl)]
      [(regexp #rx"^r (.*)$" (list _ s))         ; replace, by the x dispatch:
       (define str (string-replace s "\\n" "\n"))
       (cond
         [vis                                    ; ... the selection
          (define-values (t r c)
            (region-subst text (vis-region text vis g) (lambda (_) str)))
          (loop t (reenter t r c) script* #f (hist+ t) hl)]
         [hl                                     ; ... every highlighted run
          (define o (pos->offset text row col))
          (define t (occur-edit text hl str))
          (define p (offset->pos t (min o (string-length t))))
          (loop t (reenter t (car p) (cdr p)) script* #f (hist+ t) hl)]
         [else                                   ; ... the MODE's area at the cursor
          (M (lambda (enter h l j k target keep)
               (define-values (t r c)
                 (subst-focus text (target text 1 g)
                              (lambda (T) (string-append str (keep T)))))
               (loop t (reenter t r c) script* #f (hist+ t) hl)))])]
      [(regexp #rx"^([1-9][0-9]*)?(h|l|j|k|x|X|b|dd|d)$" (list _ n* op*))
       (define n  (if n* (string->number n*) 1))
       (define op (string->symbol op*))
       (cond
         [(and vis (memq op '(d x)))             ; delete the selection: drop its focus pieces
          (define ar (and (eq? (car vis) 'line)  ; the anchor's row, read BEFORE the edit
                          (car (land text (cadr vis)))))
          (define-values (t r0 c0)
            (region-delete text (vis-region text vis g)))
          (define-values (r c)
            (if (eq? (car vis) 'line)
                (values (min (min ar row) (sub1 (max 1 (line-count t)))) 0)
                (values r0 c0)))
          (loop t (reenter t r c) script* #f (hist+ t) hl)]
         [(eq? op 'd) (loop text g script* vis hist hl)]  ; bare d outside visual: no-op
         [(and hl (eq? op 'x))                   ; the search as cursor: x deletes every
          (define o (pos->offset text row col))  ;   highlighted run
          (define t (occur-edit text hl ""))
          (define p (offset->pos t (min o (string-length t))))
          (loop t (reenter t (car p) (cdr p)) script* #f (hist+ t) hl)]
         [(or (eq? op 'x) (and six (eq? op 'X))) ; the mode's edit TARGET, deleted: n chars
          (M (lambda (enter h l j k target keep) ;   in grid, n whole forms in sexp (mid-
               (define-values (t r c)            ;   atom the containing atom, leading ws
                 (subst-focus text (target text n g) (lambda (_) "")))   ; the next form)
               (loop t (reenter t r c) script* #f (hist+ t) hl)))]
         [(memq op '(h l j k))                   ; the mode's movement: guide -> guide
          (M (lambda (enter h l j k target keep)
               (define mv (case op [(h) h] [(l) l] [(j) j] [(k) k]))
               (loop text ((mv text n) g) script* vis hist hl)))]
         [else
          (case op
            [(X b) (define-values (t r c)
                     (if six
                         (edit-through/sexp text six sk (trim-op n))
                         (edit-through text row col (trim-op n))))
                   (loop t (reenter t r c) script* #f (hist+ t) hl)]
            [(dd) (define-values (t r) (delete-lines text row n))
                  (loop t (reenter t r 0) script* #f (hist+ t) hl)])])]
      [_ (loop text g script* vis hist hl)])))

;; ============================================================================
(module+ main
  (if (member "-i" (vector->list (current-command-line-arguments)))
      (mini-edit)
      (mini-edit #:script '("/x"          ; highlight every x (persists through what follows)
                            "s"           ; sexp mode at (0 . 0) -- the define form, spine (0)
                            "j"           ; into it: define, (0 0)
                            "l"           ; sibling: (f x), (1 0)
                            "j"           ; into it: f, (0 1 0)
                            "l"           ; sibling: x, (1 1 0)
                            "k"           ; out: back at (f x)
                            "l"           ; sibling: the body (g "ab" x), (2 0)
                            "j" "l" "l"   ; in; g -> "ab" -> x (the string is one atom)
                            "l"           ; past the last: clamps to the end slot
                            "k" "k"       ; out to the top
                            "l"           ; sibling: (h 1)
                            "/"           ; clear the highlight (with it on, x acts on IT)
                            "x"           ; delete the LAST form: the cursor lands at the
                                          ;   doc end (regression: the phantom block there)
                            "u"           ; undo it -- back before (h 1), spine (1)
                            "h"           ; back to the define form, (0)
                            "j"           ; into it: define, (0 0)
                            "l"           ; sibling: (f x), (1 0)
                            "v"           ; sexp visual: an EMPTY focus at the cut
                            "l"           ; take in a sibling: the (f x) form
                            "d"           ; delete it structurally
                            "x"           ; sexp x: delete the form now at the cursor (the body)
                            "g"           ; back to grid (the linecol alias)
                            "V" "d"       ; visual line: delete this row
                            "v" "2l" "d"  ; charwise: the 2 chars between the cuts, delete
                            "u" "u"       ; undo both: back to before the line delete
                            "s 1 0"       ; alias + address, parent first: form 1, slot 0 -- the h
                            "g 0 4"       ; alias + row/col address: (def|ine )
                            "r Z"         ; grid r: replace the char at the cursor
                            "u"           ; undo it
                            "s 1"         ; the (h 1) form whole
                            "r (k 2)"     ; sexp r: replace the form
                            "g"           ; grid again
                            "/k"          ; the search as cursor ...
                            "x"           ; ... x deletes the highlighted run
                            "u"           ; undo (the highlight persists)
                            "r zz"        ; ... r substitutes it
                            "/"           ; clear
                            "q"))))       ; quit
