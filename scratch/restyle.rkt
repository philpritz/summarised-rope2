#lang racket

;; SCRATCH -- span-run display: cut at the SEMANTIC joints, join at the
;; SYNTAX ones. The preferred pipeline is two-stage:
;;
;;   (segment (with-start g) #:merge span-merge)      one semantic cut, each
;;                                                    piece's grid start in
;;                                                    its tag
;;   (coagulate rope (over-pos v) #:merge span-merge) the theme downstream:
;;                                                    map tags to styles,
;;                                                    re-weld what the theme
;;                                                    cannot tell apart
;;
;; so line structure survives WITHOUT line cuts (spans weld across newlines
;; yet self-locate) and the renderer (render-spans) needs no walk state.
;; coagulate takes the rope fn (a make-rope factory) explicitly, per the
;; explicit-algebra convention, and shares segment's #:merge vocabulary.
;; restyle is the FUSED form of the same tag map -- the coagulation equation,
;; checked in the demo: segment (restyle v g) = coagulate v . segment g.
;; Display consumer beside render.rkt, which stands as the fused reference
;; (same wine-dark palette). The cursor-as-a-piece factor and the phantom
;; blocks live with the consumer (mini-edit2's show).

(require racket/match
         "segment.rkt"
         "../rope-core.rkt"
         "../summaries/summaries.rkt"
         "../summaries/lisp-summary.rkt"
         (submod "../summaries/lisp-summary.rkt" internal)
         (submod "../summaries/sexp-summary.rkt" internal))

(provide restyle with-head with-start span-merge face coagulate over-pos
         render-spans tint)

;; ---------- the head class enters the tag ----------
;; render.rkt's paint peeks at a code piece's first char to tell opener /
;; closer / other; a pure tag map cannot, so the head class moves into the
;; tag here: the emitted head class of the focus, read off lisp-smr the same
;; way lisp-run-guide reads it internally. (code spine) -> (code head spine).
(define (head-of bs fs)
  (define m0 (arm-exit (aref (lisp-smr bs) 0)))  ; mode at the piece's left edge
  (sexp-head (force (arm-val (aref (lisp-smr fs) (mode->i m0))))))

(define ((with-head g) bs fs as)
  (match (g bs fs as)
    [(list 'code spine) (list 'code (head-of bs fs) spine)]
    [t t]))

;; ---------- restyle: the view ----------
;; v : semantic tag -> style tag, lifted over a guide. Welding then works at
;; style coarseness: adjacent pieces v maps to one style come out one span.
(define ((restyle v) g)
  (lambda (bs fs as)
    (match (g bs fs as) [#f #f] [t (v t)])))

;; the wine-dark theme as a tag function; the style tag IS the SGR code list
;; (1 bold, 3 italic, 38 5 n = 256-color foreground n)
(define bracket-tints '(88 130 22 24 96 100))
(define (bracket-tint d) (list-ref bracket-tints (modulo d (length bracket-tints))))
(define (face t)
  (match t
    [(list 'kw      _)   '(1 38 5 89)]           ; keyword: bold mulberry
    [(list 'string  _)   '(38 5 65)]             ; string: sage-jade
    [(list 'charlit _)   '(38 5 131)]            ; charlit: rosewood
    [(list 'comment _)   '(3 38 5 60)]           ; comment: lapis slate, italic
    [(list 'block   _)   '(3 38 5 60)]
    [(list 'code 'open  spine) (list 38 5 (bracket-tint (sub1 (length spine))))]
    [(list 'code 'close spine) (list 38 5 (bracket-tint (- (length spine) 2)))]
    [(list 'code _ _)    '(38 5 252)]))          ; plain code: pearl

;; ---------- positioned spans ----------
;; tag = ((row . col) . style): the grid start read off bs's linecol at the
;; piece's left edge. span-merge is the #:merge companion: weld on style
;; equality, the left start winning -- associative (definedness included),
;; per segment's obligation.
(define ((with-start g) bs fs as)
  (match (g bs fs as)
    [#f #f]
    [t (define b (linecol-smr bs))
       (cons (cons (linecol-lines b) (linecol-cols b)) t)]))

(define (span-merge a b)
  (and (equal? (cdr a) (cdr b)) a))

;; ---------- coagulate: the equation's right-hand side ----------
;; Map v over a seglist's tags and re-weld -- the unfused form of restyle,
;; sharing segment's #:merge vocabulary (default: equal tags, kept). The
;; rope fn is taken explicitly for the joins; pieces stay ropes.
(define ((coagulate rope v #:merge [merge (lambda (a b) (and (equal? a b) a))]) segs)
  (foldr (lambda (seg tail)
           (match-define (cons tag piece) seg)
           (define t (v tag))
           (match tail
             [(cons (cons t2 p2) rest)
              (=> retry)
              (define m (merge t t2))
              (if m (cons (cons m (rope piece p2)) rest) (retry))]
             [_ (cons (cons t piece) tail)]))
         '() segs))

(define ((over-pos f) tg) (cons (car tg) (f (cdr tg))))   ; lift v past a (row . col)

;; ---------- the stateless renderer ----------
(define ESC (string (integer->char 27)))
(define (tint text cs)
  (format "~a[~am~a~a[0m" ESC (string-join (map number->string cs) ";") text ESC))

;; Every span self-locates, so there is no walk state: a span at col 0 opens
;; its line (gutter = its own row); a chunk after an interior newline opens
;; row+i; a FINAL empty chunk means the span ended with the newline -- that
;; line belongs to the next span's col-0 rule. Nothing counts anything.
(define (render-spans spans)
  (define (gutter r)
    (display (tint (format "~a | " (~a r #:min-width 3 #:align 'right)) '(38 5 59))))
  (for ([s (in-list spans)])
    (match-define (cons (cons (cons row col) codes) piece) s)
    (define chunks (string-split (format "~a" piece) "\n" #:trim? #f))
    (define last-i (sub1 (length chunks)))
    (for ([chunk (in-list chunks)] [i (in-naturals)])
      (unless (zero? i) (newline))
      (when (if (zero? i)
                (zero? col)
                (or (< i last-i) (non-empty-string? chunk)))
        (gutter (+ row i)))
      (unless (string=? chunk "") (display (tint chunk codes))))))

;; ============================================================================
(module+ main
  (define doc (string-join '("(define (fact n)      ; the classic"
                             "  (if (<= n 1)"
                             "      1"
                             "      (* n (fact (- n 1)))))"
                             ""
                             "(define greeting \"hello"
                             "world\")"
                             ""
                             "(define ch #\\()"
                             "(display greeting)")
                           "\n"))
  (define buf   (bundle char-smr linecol-smr lisp-smr atomhash-smr))
  (define R     (make-rope buf))
  (define tbl   (keyword-table "define" "lambda" "let" "if" "cond"))
  (define sem-g (with-head ((keywordize tbl) lisp-run-guide)))
  (define t     (R doc))

  (define (view segs) (map (lambda (s) (cons (car s) (format "~a" (cdr s)))) segs))
  (define sem   ((segment sem-g) t))
  (define spans ((segment ((restyle face) sem-g)) t))
  (printf "semantic pieces: ~a    styled spans: ~a~n" (length sem) (length spans))
  (printf "coagulation equation (fused view = map+reweld): ~a~n"
          (if (equal? (view spans) (view ((coagulate R face) sem))) 'HOLDS 'VIOLATED))

  (printf "~n-- the span runs: (sgr-codes . text) --~n")
  (for ([s (in-list (view spans))])
    (printf "  ~a ~s~n" (~a (car s) #:min-width 14) (cdr s)))

  (define pos-sem ((segment (with-start sem-g) #:merge span-merge) t))
  (define placed  ((coagulate R (over-pos face) #:merge span-merge) pos-sem))
  (printf "~n-- positioned: cut semantic (~a pieces), joined by style --~n"
          (length pos-sem))
  (printf "spans: ~a   content intact: ~a   agrees with the fused path: ~a~n~n"
          (length placed)
          (if (equal? (apply string-append (map (lambda (s) (format "~a" (cdr s))) placed))
                      doc)
              'yes 'NO)
          (if (equal? (view placed)
                      (view ((segment (with-start ((restyle face) sem-g))
                                      #:merge span-merge)
                             t)))
              'yes 'NO))
  (for ([s (in-list placed)])
    (match-define (cons (cons pos codes) piece) s)
    (printf "  ~a ~a ~s~n" (~a pos #:min-width 9) (~a codes #:min-width 14)
            (format "~a" piece)))

  (printf "~n-- rendered from positioned spans (gutter included, no state) --~n")
  (render-spans placed)
  (newline))
