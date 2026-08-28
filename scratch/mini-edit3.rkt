#lang racket

;; SCRATCH -- mini-edit2 with the DISPLAY rewired onto the two-stage memoized
;; segmenter (draft). mini-edit2.rkt stands as the working reference; state,
;; modes, wrapper, keymap, undo and every edit path are copied from it
;; unchanged, and still run on segment.rkt's old walk -- per the migration
;; plan in discussions/2026-07-24/1-claude.md, edit paths stay there.
;;
;; What is new is `show`, which is now the layered pipeline:
;;
;;   stage 1  MEMOIZED, changes with the text or the window
;;            (s2:segment R) over (viewport top bot SYN) -- SYN is a single
;;            persistent guide value, built once at module level: rebuilding
;;            it per frame would be a fresh (smr . fn) in the memo key and
;;            therefore zero hits.
;;   stage 2  REFINE, per keystroke, never memoized
;;            the search highlight by refine (a real guide, so it needs the
;;            scanned contexts), then the cursor or selection by refine-at
;;            (a cut set: piece lengths are intrinsic, so only the pieces
;;            holding a cut are touched).
;;   stage 3  PLACE -- grid starts folded over the finished seglist, NOT
;;            carried in stage 1's tags. A tag carrying (row . col) would go
;;            stale for every piece below an edit, which no context
;;            projection could ever rescue; keeping them downstream also
;;            leaves stage-1 tags welding on plain equal?.
;;   stage 4  COAGULATE + RENDER -- restyle.rkt's theme and stateless
;;            renderer, unchanged.
;;
;; The algebra is fast-bundle and the keyword wrapper is kw* (scratch/
;; paint-bench.rkt), which together took a cold paint from ~90 ms to ~25 ms.
;;
;; New keys beside mini-edit2's: the window follows the cursor automatically,
;; and the status line reports the paint time and the live memo size.

(require racket/match racket/string racket/list
         "segment.rkt"
         "restyle.rkt"
         "refine.rkt"
         "paint-bench.rkt"
         (prefix-in s2: "segment2.rkt")
         "../rope-core.rkt"
         "../summaries/summaries.rkt"
         "../summaries/lisp-summary.rkt"
         (submod "../summaries/lisp-summary.rkt" internal)
         (submod "../summaries/sexp-summary.rkt" internal)
         "../summaries/occur-summary.rkt"
         "../toolbox/memoize.rkt")

(provide mini-edit)

;; fast-bundle in place of bundle: same components, same protocol, so every
;; classic guide and both walks accept it unchanged.
(define buf (fast-bundle char-smr linecol-smr lisp-smr atomhash-smr occur-smr hash-smr))
(define R   (make-rope buf))
(define (before? s) (eq? (last (car s)) 'before))

(define (rope-of ps) (if (null? ps) (R "") (apply R ps)))

(define (cut-at doc g)
  (define segs ((segment (guide-product line-guide lisp-run-guide (guide->sides buf g)))
                doc))
  (splitf-at segs before?))

;; LOCATING a cursor is a guided descent, not a segmentation. mini-edit2 read
;; the position off cut-at's piece list, which costs a full old-walk pass over
;; the document EVERY time -- and show/scroll-to/the movements call it several
;; times a frame, which swamped the memoized stage entirely. multisect answers
;; the same question in O(log n).
(define (locate doc g)                           ; -> (values offset row col)
  (define-values (l _r) ((multisect buf g) doc))
  (define v (linecol-smr l))
  (values (char-smr l) (linecol-lines v) (linecol-cols v)))

(define (land doc g)       (let-values ([(_o r c) (locate doc g)]) (cons r c)))
(define (cut-offset doc g) (let-values ([(o _r _c) (locate doc g)]) o))

(define (pos->offset doc row col) (cut-offset doc (grid-at row col)))
(define (offset->pos doc n)       (land doc (char-at n)))
(define (line-count doc) (add1 (linecol-lines (linecol-smr doc))))
(define (line-len doc row) (cdr (land doc (line-end-at row))))

(define ((insert-op str) ps) (append ps (list str)))
(define ((trim-op n) ps)
  (define r (rope-of ps))
  (define k (- (char-smr r) n))
  (if (<= k 0)
      '()
      (let-values ([(pre _) (cut-at r (char-at k))]) (map cdr pre))))

;; ---------- the modes (verbatim from mini-edit2) ----------
(define (grid-mode use)
  (use
   (lambda (doc g)
     (define p (land doc g))
     (grid-at/s (car p) (cdr p)))
   (lambda (doc n) (lambda (g)
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
   (lambda (doc n) (lambda (g)
                     (match (spec-of g)
                       [(list 'grid-at r c)
                        (grid-at/s (min (sub1 (line-count doc)) (+ r n)) c)])))
   (lambda (doc n) (lambda (g)
                     (match (spec-of g)
                       [(list 'grid-at r c) (grid-at/s (max 0 (- r n)) c)])))
   (lambda (doc n g)
     (match (spec-of g)
       [(list 'grid-at r c)
        (define o (pos->offset doc r c))
        (guides->region buf g (char-at (min (char-smr doc) (+ o n))))]))
   (lambda (T) "")))

(define (spine-move ix op n)
  (case op
    [(h) (cons (max 0 (- (car ix) n)) (cdr ix))]
    [(l) (cons (+ (car ix) n) (cdr ix))]
    [(k) (let up ([ix ix] [n n])
           (if (or (zero? n) (null? (cdr ix))) ix (up (cdr ix) (sub1 n))))]
    [(j) (let down ([ix ix] [n n])
           (if (zero? n) ix (down (cons 0 ix) (sub1 n))))]))

(define (sexp-mode use)
  (define (enter doc g)                        ; the refined index off the two
    (define-values (l r) ((multisect buf g) doc))   ; sides of ONE descent
    (define-values (ix k) (nav-index l r))
    (lisp-slot-guide/s ix k))
  (define ((move op) doc n)
    (lambda (g)
      (match (spec-of g)
        [(list 'lisp-slot ix _)
         (enter doc (lisp-slot-guide/s (spine-move ix op n)))])))
  (use
   enter
   (move 'h) (move 'l) (move 'j) (move 'k)
   (lambda (doc n g)
     (match (spec-of g)
       [(list 'lisp-slot ix _)
        (define h (floor (car ix)))
        (guides->region buf (lisp-slot-guide (cons h       (cdr ix)))
                            (lisp-slot-guide (cons (+ h n) (cdr ix))))]))
   (lambda (T) (car (regexp-match #px"\\s*$" T)))))

;; ---------- region edits (verbatim from mini-edit2) ----------
(define (subst-focus doc factor f)
  (define segs ((segment (guide-product line-guide lisp-run-guide factor)) doc))
  (define (side-of s) (last (car s)))
  (define (pieces side) (for/list ([s (in-list segs)] #:when (eq? (side-of s) side)) (cdr s)))
  (define focus (string-append* (map (lambda (p) (format "~a" p)) (pieces 'focus))))
  (define o1 (for/sum ([p (in-list (pieces 'before))]) (char-smr p)))
  (define t  (rope-of (append (pieces 'before) (list (f focus)) (pieces 'after))))
  (define p  (offset->pos t (min o1 (char-smr t))))
  (values t (car p) (cdr p)))

(define (region-subst doc region f)
  (subst-focus doc (guides->region buf (car region) (cdr region)) f))
(define (region-delete doc region) (region-subst doc region (lambda (_) "")))

(define (vis-region doc vis g)
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

(define (occur-edit doc W repl)
  (define segs ((segment (occur-guide W)) doc))
  (rope-of (for/list ([s (in-list segs)])
             (if (eq? (car s) 'occur) repl (cdr s)))))

;; ---------- addresses as TEXT SOURCES ----------
;; An address already names a place to go (s 0 = the first top-level form).
;; The same address names a REGION, and a region has text -- so the argument
;; to i / r can be either a literal string or an address, and "insert what is
;; over there" needs no new vocabulary. multisect with the region's two cut
;; guides hands back the middle piece directly, so this is a guided descent,
;; not a segmentation.
(define (sexp-text doc ix)                       ; the text of the form at spine ix
  (define h (floor (car ix)))
  (define-values (_l mid _r)
    ((multisect buf (lisp-slot-guide (cons h (cdr ix)))
                    (lisp-slot-guide (cons (add1 h) (cdr ix)))) doc))
  (string-trim (format "~a" mid) #:left? #f))    ; the slot span carries the gap
                                                 ;   to the next form; drop it
;; the argument to i / r -- one small expression language over TEXT:
;;   "literal"    the string itself (read gives \n, \" etc. for free)
;;   s <spine>    the text of that form, outermost slot first, as `s` addresses
(define (text-of doc arg)
  (match arg
    [(list (? string? str))      str]
    [(list 's (? number? c) ..1) (sexp-text doc (reverse c))]
    [_                           #f]))           ; unparseable: the command no-ops
(define (delete-lines doc row n)
  (define region (vis-region doc (list 'line (grid-at row 0))
                             (grid-at (+ row (sub1 n)) 0)))
  (define-values (t _r _c) (region-delete doc region))
  (values t (min row (sub1 (max 1 (line-count t))))))

;; ---------- the state: mini-edit2's, plus the scroll row ----------
(struct st (doc cursor vis hl prev mode wrapper keymap top) #:transparent)

;; ---------- the wrapper: mode -> keymap (mini-edit2's, plus scroll keys) ----------
(define ((wrap M) . toks)
  (M (lambda (enter h l j k select keep)
       (lambda (s)
         (match-define (st doc g vis hl prev _mode wrapper _keymap top) s)
         (define-values (n rest)
           (match toks
             [(cons (? exact-positive-integer? m) r) (values m r)]
             [_                                      (values 1 toks)]))
         (define (row) (car (land doc g)))
         (define (edited t r c)
           (struct-copy st s [doc t] [cursor (enter t (grid-at/s r c))] [vis #f]
                        [prev (if (equal? (hash-smr t) (hash-smr doc)) prev s)]))
         (define (edit-at xform)
           (define gb (flip-guide g doc))
           (define-values (pre post) (cut-at doc g))
           (define t (rope-of (append (xform (map cdr pre)) (map cdr post))))
           (define p (land t gb))
           (edited t (car p) (cdr p)))
         (define (subst factor f)
           (define-values (t r c) (subst-focus doc factor f))
           (edited t r c))
         (define (hl-edit repl)
           (define o (cut-offset doc g))
           (define t (occur-edit doc hl repl))
           (define p (offset->pos t (min o (char-smr t))))
           (edited t (car p) (cdr p)))
         (define (vis-delete)
           (define lr (and (eq? (car vis) 'line)
                           (min (car (land doc (cadr vis))) (row))))
           (define-values (t r c) (region-delete doc (vis-region doc vis g)))
           (if lr
               (edited t (min lr (sub1 (max 1 (line-count t)))) 0)
               (edited t r c)))
         ;; SCROLLING moves the window and drags the cursor with it (vim's
         ;; C-e / C-y), which is the inverse of scroll-to's "window follows
         ;; cursor". Doing the clamp here leaves the cursor in view, so
         ;; scroll-to then finds nothing to do and the two never fight.
         (define (scrolled top*)
           (define t* (max 0 (min top* (sub1 (line-count doc)))))
           (define r  (row))
           (define r* (max t* (min r (+ t* WIN -1))))
           (struct-copy st s [top t*]
                        [cursor (if (= r* r)
                                    g
                                    (enter doc (grid-at/s r* (cdr (land doc g)))))]))
         (define (switch M* g0)
           (M* (lambda (enter* . _)
                 (struct-copy st s [mode M*] [keymap (wrapper M*)]
                              [cursor (enter* doc g0)] [vis #f]))))
         (match rest
           ['()       s]
           [(list 'q) #f]
           [(list 'u) (or prev s)]
           [(list (and op (or 'h 'l 'j 'k)))
            (define mv (case op [(h) h] [(l) l] [(j) j] [(k) k]))
            (struct-copy st s [cursor ((mv doc n) g)])]
           [(list 'e)  (scrolled (+ top n))]      ; the window down n rows
           [(list 'y)  (scrolled (- top n))]      ; ... and up
           [(list 'zt) (scrolled (row))]          ; put the cursor's row at the top
           [(list 'zz) (scrolled (- (row) (quotient WIN 2)))]     ; ... the middle
           [(list 'zb) (scrolled (- (row) WIN -1))]               ; ... the bottom
           [(list 0)  (struct-copy st s [cursor (enter doc (grid-at/s (row) 0))])]
           [(list '$) (struct-copy st s [cursor (enter doc (grid-at/s (row) (line-len doc (row))))])]
           [(list 'g) (switch grid-mode g)]
           [(list 'g (? number? r) (? number? c))
            (switch grid-mode (grid-at/s (min r (sub1 (line-count doc))) c))]
           [(list 's) (switch sexp-mode g)]
           [(list 's (? number? comp) ..1)
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
           [(cons 'i arg)                        ; i "text"  |  i s 0
            (define str (text-of doc arg))
            (if str (edit-at (insert-op str)) s)]
           [(cons 'r arg)                        ; r "text"  |  r s 0
            (define str (text-of doc arg))
            (cond [(not str) s]
                  [vis  (let-values ([(t r c) (region-subst doc (vis-region doc vis g)
                                                            (lambda (_) str))])
                          (edited t r c))]       ; ... the selection
                  [hl   (hl-edit str)]           ; ... every highlighted run
                  [else (subst (select doc n g)  ; ... the mode's area at the cursor
                               (lambda (T) (string-append str (keep T))))])]
           [(list (and op (or 'x 'd)))
            (cond [vis         (vis-delete)]
                  [(eq? op 'd) s]
                  [hl          (hl-edit "")]
                  [else        (subst (select doc n g) (lambda (_) ""))])]
           [(list (or 'X 'b)) (edit-at (trim-op n))]
           [(list 'dd)
            (define-values (t r) (delete-lines doc (row) n))
            (edited t r 0)]
           [_ s])))))

;; ---------- input: vim-compact OR space-separated ----------
;; mini-edit.rkt took compact commands -- "3l", "2dd", "/x", "rZ" -- which is
;; what a terminal session actually wants to type; mini-edit2 moved to
;; read-tokenized, space-separated ones ("3 l", "/ x") to get addresses like
;; "s 1 0" for free. Both are accepted here: the compact forms are tried
;; first, and anything else falls through to mini-edit2's reader, so
;; addresses and multi-word arguments are unchanged.
(define compact-op
  #px"^([1-9][0-9]*)?(dd|zt|zz|zb|h|l|j|k|e|y|x|X|b|d|v|V|u|q|g|s|0|\\$)$")
;; ONLY the search swallows the rest of the line now: a search word is bare
;; text (/define), whereas i and r take a parsed argument -- a quoted string
;; or an address -- so they are left to `read`, which gives i "a\nb" and
;; i s 0 with no special casing at all.
(define compact-arg #px"^(/)[ ]?(.*)$")

(define (read-tokens line)
  (with-handlers ([exn:fail:read? (lambda (_) '())])
    (define p (open-input-string line))
    (let loop ([toks '()])
      (define t (read p))
      (cond
        [(eof-object? t) (reverse toks)]
        [(eq? t '/)
         (define arg (port->string p))
         (reverse (cons (if (string-prefix? arg " ") (substring arg 1) arg)
                        (cons t toks)))]
        [else (loop (cons t toks))]))))

(define (tokenize line)
  (define s (string-trim line))
  (cond
    [(regexp-match compact-op s)
     => (lambda (m)
          (match-define (list _ n op) m)
          (append (if n (list (string->number n)) '())
                  (list (if (string=? op "0") 0 (string->symbol op)))))]
    [(regexp-match compact-arg s)
     => (lambda (m) (list (string->symbol (second m)) (third m)))]
    [else (read-tokens s)]))

;; ============================================================================
;; THE DISPLAY -- the two-stage memoized pipeline
;; ============================================================================
(define WIN 12)                                  ; window height, in rows
(define kw-table (keyword-table "define" "lambda" "let" "if" "cond"))

;; THE HEAD CLASS IS NOT HEREDITARY, so it must not ride a refined layer.
;; restyle.rkt's with-head reads the emitted head of the FOCUS, so splitting
;; "x " into "x" and " " changes it from 'atom to 'ws -- and refine inherits
;; the parent tag verbatim, so a piece would carry its parent's head. Caught
;; by the live -check; benign for atom/ws (face paints both as plain code)
;; but a cut inside "( " would paint the space in bracket colour.
;;
;; So the head is derived per FINAL piece, exactly as the positions are. That
;; is the general rule this session arrived at: stage 1 may only carry tag
;; components that survive subdivision; anything read off a piece's own
;; extent -- its head, its grid start -- belongs after the last cut.
(define (head-class piece)                       ; agrees with sexp-head on the
  (define s (format "~a" piece))                 ;   distinction face reads
  (cond [(zero? (string-length s))               'ws]
        [(memv (string-ref s 0) '(#\( #\[ #\{))  'open]
        [(memv (string-ref s 0) '(#\) #\] #\}))  'close]
        [(char-whitespace? (string-ref s 0))     'ws]
        [else                                    'atom]))

(define (rehead segs)
  (for/list ([s (in-list segs)])
    (match-define (cons tag piece) s)
    (match tag
      [(cons (list 'code spine) rest)
       (cons (cons (list 'code (head-class piece) spine) rest) piece)]
      [_ s])))

;; SYN and SEG are built ONCE: the memo keys the guide slot by (smr . fn), so
;; a guide rebuilt per frame never hits, and the segmenter IS the memo.
(define SYN ((kw* kw-table) (s2:lisp-run-guide buf)))
(define SEG (s2:segment R))                      ; stage 1, memoized
(define WALK (s2:segment R #:key no-memo))       ; stage 2's plain walk, hoisted

(define CHECK (and (member "-check" (vector->list (current-command-line-arguments))) #t))
(define frame-log (box '()))                     ; (syntax refine render) per frame
(define (side  i) (vector-ref #(before block after) i))
(define (vside i) (vector-ref #(before focus after) i))   ; a selection's three parts

;; stage 3: grid starts folded over the FINISHED seglist
(define (place segs)
  (for/list ([s (in-list segs)] [b (in-list (scanl linecol-smr (map cdr segs)))])
    (cons (cons (cons (linecol-lines b) (linecol-cols b)) (car s)) (cdr s))))

(define (face* tag)
  (match-define (cons ltag rest) tag)
  (cond [(memq 'block rest) '(7)]                          ; the cursor's own char
        [(symbol? ltag)     '(2 38 5 60)]                  ; a hidden-region blob
        [else (append (face ltag)
                      (cond [(memq 'focus rest) '(48 5 238)]
                            [(memq 'occur rest) '(48 5 58)]
                            [else               '()]))]))

;; a blob's text is replaced by a marker AFTER placing, so the rows stamped on
;; everything below it are the document's real rows
;; A hidden region is ONE marker however the fast layers cut it -- the search
;; refine will happily cut a blob at every match, and those cuts are correct
;; (they are the same cuts the fused walk would make); they just must not each
;; become their own "lines hidden" line. So coalesce the run.
(define (blob? s) (symbol? (car (cdr (car s)))))          ; tag = (pos ltag . rest)

(define (markers segs)
  (let loop ([segs segs] [acc '()])
    (cond
      [(null? segs) (reverse acc)]
      [(blob? (car segs))
       (define-values (run rest) (splitf-at segs blob?))
       (define v (apply linecol-smr (map cdr run)))
       (loop rest (cons (cons (car (first run))                ; the run's own start
                              (format "(~a lines hidden)\n"    ; a blob need not end
                                      (+ (linecol-lines v)     ;   on a newline
                                         (if (zero? (linecol-cols v)) 0 1))))
                        acc))]
      [else (loop (cdr segs) (cons (car segs) acc))])))

(define (phantom spans)
  (for/list ([s (in-list spans)])
    (match s
      [(cons (and tg (cons _ '(7))) piece)
       #:when (regexp-match? #rx"^\n" (format "~a" piece))
       (cons tg (string-append " " (format "~a" piece)))]
      [_ s])))

(define (show s)
  (match-define (st doc g vis hl _prev _mode _wrapper _keymap top) s)
  (printf "~n")
  (define p   (cut-offset doc g))
  (define t0 (current-inexact-milliseconds))
  (define bot (+ top WIN -1))
  ;; ---- stage 1: memoized syntax x viewport
  (define segs1 (SEG doc (s2:viewport top bot SYN)))
  (define t1 (current-inexact-milliseconds))
  ;; ---- stage 2: the fast layers, list-local, never memoized
  (define segs2                                  ; the search: a real guide
    (if hl
        ((refine R occur-smr (s2:occur-guide hl) #:tag list #:walk WALK) segs1)
        segs1))
  (define first? (not hl))                       ; has a tag list been opened yet?
  ;; both the cursor and a selection are CUT SETS -- two offsets, located by
  ;; one guided descent each -- so neither needs a guide or the scans: only
  ;; the pieces holding a cut are touched, the rest keep their tag.
  (define (cuts+side)
    (if vis
        (let* ([r  (vis-region doc vis g)]
               [o1 (cut-offset doc (car r))]
               [o2 (cut-offset doc (cdr r))])
          (values (list (min o1 o2) (max o1 o2)) vside))
        (values (list p (add1 p)) side)))
  (define-values (cs mark) (cuts+side))
  (define segs3
    ((refine-at R char-smr cs
                #:tag (if first?
                          (lambda (t i) (list t (mark i)))
                          (lambda (t i) (append t (list (mark i)))))) segs2))
  (define t2 (current-inexact-milliseconds))
  ;; ---- the refinement law, checked live under -check: the two staged cuts
  ;; must equal the ONE fused product walk (viewport x syntax x search x cursor)
  (when CHECK
    (define factors
      (append (list (s2:viewport top bot SYN))
              (if hl (list (s2:occur-guide hl)) '())
              (list (if vis
                        (let ([r (vis-region doc vis g)])
                          (s2:guides->region buf (car r) (cdr r)))
                        (cursor-block-at p)))))
    (define fused (WALK doc (apply s2:guide-product buf factors)))
    (define (view segs) (map (lambda (s) (cons (car s) (format "~a" (cdr s)))) segs))
    (unless (equal? (view segs3) (view fused))
      (printf "!! two-stage DISAGREES with the fused walk (~a vs ~a pieces)~n"
              (length segs3) (length fused))
      (for/first ([a (in-list (view segs3))] [b (in-list (view fused))] [i (in-naturals)]
                  #:unless (equal? a b))
        (printf "   first difference at piece ~a~n     two-stage ~s~n     fused     ~s~n"
                i a b))))
  ;; ---- stages 3 and 4: place, theme, render
  (define spans ((coagulate R (over-pos face*) #:merge span-merge)
                 (markers (place (rehead segs3)))))
  (render-spans (phantom spans))
  (when (and (not vis) (= p (char-smr doc)) (<= (car (land doc g)) bot))
    (display (tint " " '(7))))
  (newline)
  (define t3 (current-inexact-milliseconds))
  (set-box! frame-log (cons (list (- t1 t0) (- t2 t1) (- t3 t2)) (unbox frame-log)))
  (define flags (string-append (if hl (format "  / ~a" hl) "")
                               (match vis
                                 [(list kind _) (format "  VISUAL ~a" kind)]
                                 [#f            ""])))
  (printf "[rows ~a-~a  paint ~ams = syntax ~a + refine ~a + render ~a   memo ~a]~n"
          top bot
          (~r (- t3 t0) #:precision 2) (~r (- t1 t0) #:precision 2)
          (~r (- t2 t1) #:precision 2) (~r (- t3 t2) #:precision 2)
          (memo-size SEG))
  (match (spec-of g)
    [(list 'lisp-slot ix k)
     (define at (land doc g))
     (printf "[SEXP ~a:~a ~a+~a~a]  h/l sibs  k out  j in  e/y zz scroll  g grid~n> "
             (car at) (cdr at) ix k flags)]
    [(list 'grid-at r c)
     (printf "[~a:~a~a]  h/l k/j 0/$  e/y zt/zz/zb  s sexp  v/V  i/r \"t\"|s N  x/X/dd  / hl  u  q~n> "
             r c flags)]))

;; the window follows the cursor -- the only new state, and it never enters
;; the memo key (a covering viewport is stripped)
(define (scroll-to s)
  (define r (car (land (st-doc s) (st-cursor s))))
  (define top (st-top s))
  (struct-copy st s [top (cond [(< r top)              r]
                               [(> r (+ top WIN -1))   (- r WIN -1)]
                               [else                   top])]))

;; ---------- the loop ----------
(define default-text
  (string-join
   '("(define (fact n)      ; the classic"
     "  (if (<= n 1)"
     "      1"
     "      (* n (fact (- n 1)))))"
     ""
     "(define (fib n)"
     "  (cond [(< n 2) n]"
     "        [else (+ (fib (- n 1))"
     "                 (fib (- n 2)))]))"
     ""
     "(define greeting \"hello"
     "world\")"
     ""
     "(define (main)"
     "  (display greeting)"
     "  (display (fact 5))"
     "  (display (fib 10)))"
     ""
     "(main)")
   "\n"))

(define (mini-edit [text0 default-text] #:script [script0 #f])
  (define s0 (st (R text0) (grid-at/s 0 0) #f #f #f
                 grid-mode wrap (wrap grid-mode) 0))
  (let loop ([s s0] [script script0])
    (show s)
    (define cmd
      (cond [(not script)
             (let ([l (read-line (current-input-port) 'any)])
               (if (eof-object? l) "q" (string-trim l "\r" #:left? #f)))]
            [(null? script) "q"]
            [else (car script)]))
    (when script (printf "~a~n" cmd))
    (define s* ((apply (st-keymap s) (tokenize cmd)) s))
    (if s*
        (loop (scroll-to s*) (if (pair? script) (cdr script) script))
        (let* ([fs (reverse (unbox frame-log))]
               [n  (length fs)]
               [col (lambda (i) (map (lambda (f) (list-ref f i)) fs))]
               [med (lambda (xs) (let ([v (sort xs <)]) (list-ref v (quotient (length v) 2))))])
          (printf "bye~n~n")
          (printf "~a frames.  median per frame:  syntax ~a ms   refine ~a ms   render ~a ms~n"
                  n (~r (med (col 0)) #:precision 2) (~r (med (col 1)) #:precision 2)
                  (~r (med (col 2)) #:precision 2))
          (printf "first paint (cold): syntax ~a ms    memo entries at exit: ~a~n"
                  (~r (car (col 0)) #:precision 2) (memo-size SEG))))))

;; ============================================================================
(module+ main
  (if (member "-i" (vector->list (current-command-line-arguments)))
      (mini-edit)
      (mini-edit #:script '("6 j"        ; scroll down: the window follows
                            "6 j"        ; ... again, into the strings
                            "4 j"        ; ... to the bottom
                            "8 k"        ; back up: every window already cached
                            "l" "l" "l"  ; cursor only: stage 1 must be all hits
                            "/ define"   ; the search layer, refined in stage 2
                            "j" "j"      ; ... cursor moves under it
                            "/"          ; clear
                            "s"          ; sexp mode
                            "j" "l" "j"  ; structural movement
                            "k" "k"      ; out
                            "g"          ; grid again
                            "v" "3 l" "d"   ; visual cut-wise, delete
                            "u"          ; undo -- an EDIT: stage 1 recomputes
                            "i \"xy\""   ; insert a literal
                            "u"
                            "0"
                            "i s 0"      ; insert the FIRST top-level form's text
                            "u"          ;   -- the address doubles as a source
                            "s 2"        ; navigate by the same address language
                            "r s 0"      ; ... and replace form 2 with form 0
                            "u" "g"
                            "q"))))
