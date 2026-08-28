#lang racket

;; SCRATCH -- mini-edit restructured (successor sketch to mini-edit.rkt; that
;; file stands as the working reference). The editor splits in two:
;;
;;   STATE (struct st) -- the document, every guide pointing into it, and the
;;   machinery that interprets keys:
;;     doc      the document ROPE -- persistent across commands: edits
;;              reassemble it from the cut's pieces, so untouched subtrees
;;              share structure and their summaries stay cached
;;     cursor   THE cursor: a guide/s -- callable to cut the text, its spec
;;              readable; the mode's coordinate system lives in the spec
;;     vis      selection: #f | (list 'cut|'line anchor-guide)
;;     hl       search highlight: #f | word
;;     prev     the whole pre-edit state; undo just follows the chain (its tail
;;              IS the rest of the history; movements don't push)
;;     mode     the current mode (church tuple, below)
;;     wrapper  mode -> keymap, kept in the state so switching can rebuild
;;     keymap   (wrapper mode), cached: tok ... -> (state -> state | #f-quit)
;;
;;   MODE -- only what genuinely differs per mode, church-encoded:
;;     mode : (enter h l j k select keep -> A) -> A
;;     enter   : doc guide -> guide        my cursor at ANY guide's cut: split
;;                                         the text with it, read my address off
;;                                         the summaries at the gap
;;     h/l/j/k : doc n -> guide -> guide   movements rewrite the cursor's spec
;;                                         (sexp normalizes through the doc, so
;;                                         unreal spines never enter the state)
;;     select  : doc n guide -> seg-guide  the region x/r act on (n chars in
;;                                         grid, n whole forms in sexp)
;;     keep    : focus-text -> string      what r preserves (sexp: trailing ws)
;;
;; Everything generic -- insert, delete, replace, backspace, visual selection,
;; search, undo, mode switching, dd -- lives in the WRAPPER, defined once
;; against the mode's slots. A visual selection is generic too: it IS the
;; region between two cut guides (the stored anchor and the cursor), so
;; charwise grid and structural sexp selection are one 'cut kind; only
;; linewise V stays special (row-inclusive coordinates, not cuts) -- and dd is
;; its degenerate case, the line region at the cursor alone. At-cursor edits
;; (insert, backspace) re-find the cursor with the plain char flip
;; (flip-guide): the edit leaves the back name unchanged, so the per-mode
;; flips (flip-linecol / flip-sexp) are not needed here.
;;
;; Commands are READ-TOKENIZED into symbols and numbers, space-separated --
;; counts split off: "3 l", "2 dd", "s 1 0"; after i / r / / the REST of the
;; line is one string argument ("i ab cd"; \n escapes a newline). Keys:
;;   [n] h/l/j/k   the mode's movements        0 / $   line start / end
;;   i <t>         insert at the cursor
;;   r <t>         replace: the selection if one is active, every highlighted
;;                 run if / is on, else the mode's area at the cursor
;;   [n] x         delete, by the same dispatch as r
;;   [n] X | b     backspace                   [n] dd  delete whole lines
;;   v / V         visual, cut-wise / line-wise: movements extend it, d (or x)
;;                 deletes it, the same key again cancels
;;   / <w>         highlight every <w>; / alone clears
;;   g | s [addr]  switch mode, optionally AT an address (g row col; s the
;;                 spine, OUTERMOST slot first)
;;   u             undo (moves are not edits)  q       quit
;; Dropped from mini-edit.rkt: X as a sexp form-delete alias (X = backspace
;; uniformly; x already deletes forms in sexp mode).

(require racket/match racket/string racket/list
         "segment.rkt"
         "restyle.rkt"
         "../rope-core.rkt"
         "../summaries/summaries.rkt"
         "../summaries/lisp-summary.rkt"
         "../summaries/occur-summary.rkt")

(provide mini-edit)

;; occur-smr rides along for the search factor (its value is queried lazily,
;; per word, so it costs nothing until / is used); hash-smr for the cheap
;; did-the-edit-change-anything fingerprint check.
(define buf (bundle char-smr linecol-smr lisp-smr atomhash-smr occur-smr hash-smr))
(define R   (make-rope buf))                     ; the rope fn, threaded explicitly
(define (before? s) (eq? (last (car s)) 'before))

(define (rope-of ps)                             ; pieces (ropes / strings) -> one rope
  (if (null? ps) (R "") (apply R ps)))

(define (cut-at doc g)                           ; -> (values pre post) piece lists
  (define segs ((segment (guide-product line-guide lisp-run-guide (guide->sides buf g)))
                doc))
  (splitf-at segs before?))

(define (pos-of pre)                             ; (row . col) of the cut, off the summaries
  (if (null? pre)
      (cons 0 0)
      (let ([v (apply linecol-smr (map cdr pre))])
        (cons (linecol-lines v) (linecol-cols v)))))

(define (land doc g)                             ; land ANY callable guide: (row . col)
  (define-values (pre _) (cut-at doc g))
  (pos-of pre))

(define (cut-offset doc g)                       ; chars left of a guide's cut
  (define-values (pre _) (cut-at doc g))
  (for/sum ([p (in-list pre)]) (char-smr (cdr p))))

(define (pos->offset doc row col) (cut-offset doc (grid-at row col)))
(define (offset->pos doc n)       (land doc (char-at n)))

(define (line-count doc) (add1 (linecol-lines (linecol-smr doc))))
(define (line-len doc row) (cdr (land doc (line-end-at row))))   ; col AT the line's end

(define ((insert-op str) ps) (append ps (list str)))
(define ((trim-op n) ps)                         ; drop n chars from the end of the pieces
  (define r (rope-of ps))
  (define k (- (char-smr r) n))
  (if (<= k 0)
      '()
      (let-values ([(pre _) (cut-at r (char-at k))]) (map cdr pre))))

;; ---------- the modes ----------
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
                        (define p (offset->pos doc (min (char-smr doc)
                                                        (+ (pos->offset doc r c) n))))
                        (grid-at/s (car p) (cdr p))])))
   (lambda (doc n) (lambda (g)                 ; j/k: rows, the column kept as a GOAL
                     (match (spec-of g)
                       [(list 'grid-at r c)
                        (grid-at/s (min (sub1 (line-count doc)) (+ r n)) c)])))
   (lambda (doc n) (lambda (g)
                     (match (spec-of g)
                       [(list 'grid-at r c) (grid-at/s (max 0 (- r n)) c)])))
   (lambda (doc n g)                           ; select: the n chars right of the cursor
     (match (spec-of g)
       [(list 'grid-at r c)
        (define o (pos->offset doc r c))
        (guides->region buf g (char-at (min (char-smr doc) (+ o n))))]))
   (lambda (T) "")))                           ; replacement is exact

(define (spine-move ix op n)                     ; h/l: siblings; k: out; j: in
  (case op
    [(h) (cons (max 0 (- (car ix) n)) (cdr ix))]
    [(l) (cons (+ (car ix) n) (cdr ix))]
    [(k) (let up ([ix ix] [n n])
           (if (or (zero? n) (null? (cdr ix))) ix (up (cdr ix) (sub1 n))))]
    [(j) (let down ([ix ix] [n n])
           (if (zero? n) ix (down (cons 0 ix) (sub1 n))))]))

(define (sexp-mode use)
  (define (enter doc g)                        ; enter: read the gap's summaries as a
    (define-values (pre post) (cut-at doc g))  ;   refined index
    (define-values (ix k)
      (nav-index (apply buf (map cdr pre)) (apply buf (map cdr post))))
    (lisp-slot-guide/s ix k))
  (define ((move op) doc n)                    ; hjkl: spine arithmetic, then NORMALIZE
    (lambda (g)                                ;   by landing (= enter): an unreal spine
      (match (spec-of g)                       ;   (j into an atom, l past the end) lands
        [(list 'lisp-slot ix _)                ;   at a real cut and reads back as it, so
         (enter doc                            ;   the spec never drifts from the text
                (lisp-slot-guide/s (spine-move ix op n)))])))
  (use
   enter
   (move 'h) (move 'l) (move 'j) (move 'k)
   (lambda (doc n g)                           ; select: the n whole forms at the cursor
     (match (spec-of g)
       [(list 'lisp-slot ix _)
        (define h (floor (car ix)))
        (guides->region buf (lisp-slot-guide (cons h       (cdr ix)))
                            (lisp-slot-guide (cons (+ h n) (cdr ix))))]))
   (lambda (T) (car (regexp-match #px"\\s*$" T)))))   ; keep the form's trailing ws

;; ---------- region edits (shared by selections, x/r targets, dd) ----------
(define (subst-focus doc factor f)               ; factor: a 3-tag seg-guide;
  (define segs ((segment (guide-product line-guide lisp-run-guide factor)) doc))
  (define (side-of s) (last (car s)))            ; f : focus text -> replacement
  (define (pieces side) (for/list ([s (in-list segs)] #:when (eq? (side-of s) side)) (cdr s)))
  (define focus (string-append* (map (lambda (p) (format "~a" p)) (pieces 'focus))))
  (define o1 (for/sum ([p (in-list (pieces 'before))]) (char-smr p)))
  (define t  (rope-of (append (pieces 'before) (list (f focus)) (pieces 'after))))
  (define p  (offset->pos t (min o1 (char-smr t))))
  (values t (car p) (cdr p)))                    ; -> (values doc1 row col), cursor at
                                                 ;    the focus's start
(define (region-subst doc region f)
  (subst-focus doc (guides->region buf (car region) (cdr region)) f))
(define (region-delete doc region) (region-subst doc region (lambda (_) "")))

;; a selection is the REGION between two cut guides -- 'cut covers charwise
;; grid AND structural sexp selection (the guides carry the mode); 'line is
;; row-inclusive, vim dd semantics: the right cut is the next row's start,
;; and deleting through the END of the text takes the PRECEDING newline
(define (vis-region doc vis g)                   ; -> (cons gl gr)
  (match vis
    [(list 'line ga)
     (let* ([ra (car (land doc ga))] [rc (car (land doc g))]
            [r1 (min ra rc)]         [r2 (max ra rc)])
       (if (and (= r2 (sub1 (line-count doc))) (> r1 0))
           (cons (line-end-at (sub1 r1)) (char-at (char-smr doc)))
           (cons (grid-at r1 0) (grid-at (add1 r2) 0))))]
    [(list 'cut ga)
     (if (<= (cut-offset doc ga) (cut-offset doc g))
         (cons ga g)
         (cons g ga))]))

;; the search as a cursor: with /<word> on, x and r act on the OCCUR pieces
(define (occur-edit doc W repl)
  (define segs ((segment (occur-guide W)) doc))
  (rope-of (for/list ([s (in-list segs)])
             (if (eq? (car s) 'occur) repl (cdr s)))))

(define (delete-lines doc row n)                 ; [n]dd = the degenerate line REGION
  (define region (vis-region doc (list 'line (grid-at row 0))
                             (grid-at (+ row (sub1 n)) 0)))
  (define-values (t _r _c) (region-delete doc region))
  (values t (min row (sub1 (max 1 (line-count t))))))

;; ---------- the state ----------
(struct st (doc cursor vis hl prev mode wrapper keymap) #:transparent)

;; ---------- the wrapper: mode -> keymap ----------
;; keymap : tok ... -> (state -> state | #f-quit). One [count] prefix is
;; normalized off the front (default 1); everything below is written against
;; the mode's slots alone.
(define ((wrap M) . toks)
  (M (lambda (enter h l j k select keep)
       (lambda (s)
         (match-define (st doc g vis hl prev _mode wrapper _keymap) s)
         (define-values (n rest)
           (match toks
             [(cons (? exact-positive-integer? m) r) (values m r)]
             [_                                      (values 1 toks)]))
         (define (row) (car (land doc g)))
         (define (edited t r c)                  ; a completed edit: land, push undo
           (struct-copy st s [doc t] [cursor (enter t (grid-at/s r c))] [vis #f]
                        [prev (if (equal? (hash-smr t) (hash-smr doc)) prev s)]))
         (define (edit-at xform)                 ; at-cursor edit through the char flip
           (define gb (flip-guide g doc))
           (define-values (pre post) (cut-at doc g))
           (define t (rope-of (append (xform (map cdr pre)) (map cdr post))))
           (define p (land t gb))
           (edited t (car p) (cdr p)))
         (define (subst factor f)
           (define-values (t r c) (subst-focus doc factor f))
           (edited t r c))
         (define (hl-edit repl)                  ; every highlighted run at once
           (define o (cut-offset doc g))
           (define t (occur-edit doc hl repl))
           (define p (offset->pos t (min o (char-smr t))))
           (edited t (car p) (cdr p)))
         (define (vis-delete)
           (define lr (and (eq? (car vis) 'line) ; surviving row, read pre-edit
                           (min (car (land doc (cadr vis))) (row))))
           (define-values (t r c) (region-delete doc (vis-region doc vis g)))
           (if lr
               (edited t (min lr (sub1 (max 1 (line-count t)))) 0)
               (edited t r c)))
         (define (switch M* g0)                  ; enter the new mode at a guide's cut
           (M* (lambda (enter* . _)
                 (struct-copy st s [mode M*] [keymap (wrapper M*)]
                              [cursor (enter* doc g0)] [vis #f]))))
         (match rest
           ['()       s]
           [(list 'q) #f]
           [(list 'u) (or prev s)]
           [(list (and op (or 'h 'l 'j 'k)))     ; the mode's movement: guide -> guide
            (define mv (case op [(h) h] [(l) l] [(j) j] [(k) k]))
            (struct-copy st s [cursor ((mv doc n) g)])]
           [(list 0)  (struct-copy st s [cursor (enter doc (grid-at/s (row) 0))])]
           [(list '$) (struct-copy st s [cursor (enter doc (grid-at/s (row) (line-len doc (row))))])]
           [(list 'g) (switch grid-mode g)]
           [(list 'g (? number? r) (? number? c))
            (switch grid-mode (grid-at/s (min r (sub1 (line-count doc))) c))]
           [(list 's) (switch sexp-mode g)]
           [(list 's (? number? comp) ..1)       ; a spine, OUTERMOST slot first
            (switch sexp-mode (lisp-slot-guide/s (reverse comp)))]
           [(list 'v) (struct-copy st s [vis (match vis
                                               [#f              (list 'cut g)]
                                               [(list 'line ga) (list 'cut ga)]
                                               [_               #f])])]
           [(list 'V) (struct-copy st s [vis (match vis
                                               [#f              (list 'line g)]
                                               [(list 'cut ga)  (list 'line ga)]
                                               [_               #f])])]
           [(list '/ w) (struct-copy st s [hl (and (non-empty-string? w) w)])]
           [(list 'i str)
            (edit-at (insert-op (string-replace str "\\n" "\n")))]
           [(list 'r str0)                       ; replace, by the x dispatch:
            (define str (string-replace str0 "\\n" "\n"))
            (cond [vis  (let-values ([(t r c) (region-subst doc (vis-region doc vis g)
                                                            (lambda (_) str))])
                          (edited t r c))]       ; ... the selection
                  [hl   (hl-edit str)]           ; ... every highlighted run
                  [else (subst (select doc n g)  ; ... the mode's area at the cursor
                               (lambda (T) (string-append str (keep T))))])]
           [(list (and op (or 'x 'd)))
            (cond [vis         (vis-delete)]     ; delete the selection
                  [(eq? op 'd) s]                ; bare d outside visual: no-op
                  [hl          (hl-edit "")]
                  [else        (subst (select doc n g) (lambda (_) ""))])]
           [(list (or 'X 'b)) (edit-at (trim-op n))]
           [(list 'dd)
            (define-values (t r) (delete-lines doc (row) n))
            (edited t r 0)]
           [_ s])))))

;; ---------- input: read-tokenize a command line ----------
;; "3 l" -> (3 l); "s 1 0" -> (s 1 0); after i / r / /, the rest of the line
;; is ONE string argument (the single separating space dropped): "i ab cd" ->
;; (i "ab cd"), "/" -> (/ "").
(define (tokenize line)
  (with-handlers ([exn:fail:read? (lambda (_) '())])
    (define p (open-input-string line))
    (let loop ([toks '()])
      (define t (read p))
      (cond
        [(eof-object? t) (reverse toks)]
        [(memq t '(i r /))
         (define arg (port->string p))
         (reverse (cons (if (string-prefix? arg " ") (substring arg 1) arg)
                        (cons t toks)))]
        [else (loop (cons t toks))]))))

;; ---------- display: cut at the semantic joints, joined at the syntax ones ----------
;; One positioned SEMANTIC segmentation (no line factor -- line structure
;; rides the tags' grid starts); coagulate re-welds whatever face* cannot
;; tell apart; render-spans prints, stateless. The cursor is a PIECE
;; (cursor-block-at), so the block is a tag like any other; only the phantom
;; blocks -- the cursor ON a newline or past the doc end, where the gap has
;; no glyph of its own -- stay positional.
(define kw-table (keyword-table "define" "lambda" "let" "if" "cond"))

(define ((cursor-block-at p) bs fs as)           ; the cursor's char as its own piece
  (let ([b (char-smr bs)] [f (char-smr fs)])
    (cond [(<= (+ b f) p)        'before]
          [(>= b (add1 p))       'after]
          [(and (= b p) (= f 1)) 'block]
          [else                  #f])))

(define (face* tag)                              ; composite tag -> style: the face,
  (match-define (cons ltag rest) tag)            ;   over the side/occ backgrounds
  (if (memq 'block rest)
      '(7)                                       ; the cursor char: reverse video
      (append (face ltag)
              (cond [(memq 'focus rest) '(48 5 238)]
                    [(memq 'occur rest) '(48 5 58)]
                    [else               '()]))))

(define (phantom spans)                          ; a block ON a newline renders as a
  (for/list ([s (in-list spans)])                ;   reversed space before the break
    (match s
      [(cons (and tg (cons _ '(7))) piece)
       #:when (regexp-match? #rx"^\n" (format "~a" piece))
       (cons tg (string-append " " (format "~a" piece)))]
      [_ s])))

(define (show s)
  (match-define (st doc g vis hl _prev _mode _wrapper _keymap) s)
  (printf "~n")
  (define p (cut-offset doc g))
  (define factors                                ; the semantic product
    (append (list (with-head ((keywordize kw-table) lisp-run-guide)))
            (if hl (list (occur-guide hl)) '())
            (if vis
                (let ([r (vis-region doc vis g)])
                  (list (guides->region buf (car r) (cdr r))))
                (list (cursor-block-at p)))))
  (define pos-sem                                ; CUT: the semantic joints, placed
    ((segment (with-start (apply guide-product factors)) #:merge span-merge) doc))
  (define spans                                  ; JOIN: what the theme can't tell apart
    ((coagulate R (over-pos face*) #:merge span-merge) pos-sem))
  (render-spans (phantom spans))
  (when (and (not vis) (= p (char-smr doc)))     ; no char to tag past the end
    (display (tint " " '(7))))
  (newline)
  (define flags (string-append (if hl (format "  / ~a" hl) "")
                               (match vis
                                 [(list kind _) (format "  VISUAL ~a" kind)]
                                 [#f            ""])))
  (match (spec-of g)
    [(list 'lisp-slot ix k)
     (define at (land doc g))
     (printf "[SEXP ~a:~a ~a+~a~a]  h/l sibs  k out  j in  g grid~n> "
             (car at) (cdr at) ix k flags)]
    [(list 'grid-at r c)
     (printf "[~a:~a~a]  h/l k/j 0/$  s sexp  v/V  i/r/x/X/dd  / hl  u  q~n> "
             r c flags)]))

;; ---------- the loop ----------
(define (mini-edit [text0 "(define (f x)\n  (g \"ab\" x))\n(h 1)"] #:script [script0 #f])
  (define s0 (st ((make-rope buf) text0) (grid-at/s 0 0) #f #f #f
                 grid-mode wrap (wrap grid-mode)))
  (let loop ([s s0] [script script0])
    (show s)
    (define cmd
      (cond [(not script)                        ; 'any: eat \r\n whole -- a Windows console
             (let ([l (read-line (current-input-port) 'any)])   ; line otherwise arrives "j\r"
               (if (eof-object? l) "q" (string-trim l "\r" #:left? #f)))]
            [(null? script) "q"]
            [else (car script)]))
    (when script (printf "~a~n" cmd))
    (define s* ((apply (st-keymap s) (tokenize cmd)) s))
    (if s*
        (loop s* (if (pair? script) (cdr script) script))
        (printf "bye~n"))))

;; ============================================================================
(module+ main
  (if (member "-i" (vector->list (current-command-line-arguments)))
      (mini-edit)
      (mini-edit #:script '("/ x"        ; highlight every x (persists through what follows)
                            "s"          ; sexp mode at (0 . 0) -- the define form, spine (0)
                            "j"          ; into it: define, (0 0)
                            "j"          ; into an ATOM: the unreal (0 0 0) lands at the
                                         ;   same cut and normalizes back -- stays (0 0)
                            "l"          ; sibling: (f x), (1 0)
                            "j"          ; into it: f, (0 1 0)
                            "l"          ; sibling: x, (1 1 0)
                            "k"          ; out: back at (f x)
                            "l"          ; sibling: the body (g "ab" x), (2 0)
                            "j" "l" "l"  ; in; g -> "ab" -> x (the string is one atom)
                            "l"          ; past the last: clamps to the end slot
                            "k" "k"      ; out to the top
                            "l"          ; sibling: (h 1)
                            "/"          ; clear the highlight (with it on, x acts on IT)
                            "x"          ; delete the LAST form: the cursor lands at the
                                         ;   doc end (regression: the phantom block there)
                            "u"          ; undo it -- back before (h 1), spine (1)
                            "h"          ; back to the define form, (0)
                            "j"          ; into it: define, (0 0)
                            "l"          ; sibling: (f x), (1 0)
                            "v"          ; visual ('cut): an EMPTY focus at the cut
                            "l"          ; take in a sibling: the (f x) form
                            "d"          ; delete it structurally
                            "x"          ; sexp x: delete the form now at the cursor (the body)
                            "g"          ; back to grid mode
                            "V" "d"      ; visual line: delete this row
                            "v" "2 l" "d"   ; cut-wise: the 2 chars between the cuts, delete
                            "u" "u"      ; undo both: back to before the line delete
                            "s 1 0"      ; address, parent first: form 1, slot 0 -- the h
                            "g 0 4"      ; row/col address: (def|ine )
                            "r Z"        ; grid r: replace the char at the cursor
                            "u"          ; undo it
                            "s 1"        ; the (h 1) form whole
                            "r (k 2)"    ; sexp r: replace the form
                            "g"          ; grid again
                            "/ k"        ; the search as cursor ...
                            "x"          ; ... x deletes the highlighted run
                            "u"          ; undo (the pre-edit state carried the highlight)
                            "r zz"       ; ... r substitutes it
                            "/"          ; clear
                            "q"))))      ; quit
