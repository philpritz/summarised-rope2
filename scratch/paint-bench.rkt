#lang racket

;; SCRATCH -- making the window PAINT cheaper. Draft; a bench, not a design.
;;
;; A cold paint (= a first paint, a leap, AND every keystroke, since an edit
;; invalidates every entry in the window) cost ~83 ms / 2375 steps / 35 us
;; per step at an 80-row window. Profiling that showed the time is NOT in the
;; guides: it is in the summary PLUMBING --
;;
;;   algebra.rkt:138  variadic/spread application   21.5% self
;;   summaries.rkt:66 the bundle's per-join hasheq  16.8% self
;;   hash-has-key?    bundle slot lookup             9.2% self
;;   part->summary    generic-interface dispatch     7.0% self
;;   coerce           rope-core's argument coercion  6.6% self
;;   ---------------------------------------------- ~61%
;;   lisp+ 3.9%, fp-leaf/fp-join 4.4%, tok-join 1.7%, measure 1.7%  = the
;;   actual summary arithmetic, ~12%.
;;
;; So the three prototypes here attack plumbing, not algorithms:
;;
;;   fast-bundle       a VECTOR-backed bundle value: no per-join hasheq, no
;;                     hash-has-key?, no generic dispatch, and a case-lambda
;;                     application with 1- and 2-argument fast paths instead
;;                     of the variadic fold. Same protocol (gen:summary-part),
;;                     so every existing guide and rope-core itself accept it.
;;   kw*               keywordize written natively for the framed algebra.
;;                     segment2's version bridges through as-old, so EVERY
;;                     judged focus pays a frame -- two bundle joins and a
;;                     struct-copy -- purely to hand the same values back.
;;   lisp-run-guide*   segment.rkt's lisp guide with the second spine and the
;;                     head class computed LAZILY: both are discarded on the
;;                     single-char path, which a plain let evaluates anyway.
;;
;; Everything here is a prototype in scratch: promoting fast-bundle means
;; changing summaries.rkt (and, for the variadic fast path, toolbox/algebra.rkt
;; or rope-core's make-summary), which is why it is measured here first.

(require racket/match racket/list
         (except-in "../rope-core.rkt" frame)
         (submod "../rope-core.rkt" internal)
         "../summaries/summaries.rkt"
         "../summaries/lisp-summary.rkt"
         (submod "../summaries/lisp-summary.rkt" internal)
         (submod "../summaries/sexp-summary.rkt" internal)
         "../toolbox/memoize.rkt"
         (only-in "../toolbox/algebra.rkt" variadic spread)
         "segment2.rkt"
         "refine.rkt"
         (prefix-in old: "segment.rkt"))

(provide fast-bundle kw* lisp-run-guide*)

;; ---------- fast-bundle: the bundle value, vector-backed ----------
;; summaries.rkt's bundle allocates a hasheq of component->value at EVERY
;; join and reads slots through gen:summary-part's generic dispatch plus a
;; hash-has-key?. With at most a handful of components a vector and an eq?
;; scan beat both, and the scan is over the same fixed vector every time.
(struct fbv (owner comps vals)
  #:methods gen:summary-part
  [(define (part->summary bv smr [fail (lambda ()
                                         (error 'part->summary "no slot in ~v" bv))])
     (define cs (fbv-comps bv))
     (define n (vector-length cs))
     (let loop ([i 0])
       (cond [(eq? i n)
              (cond [(eq? smr ((fbv-owner bv))) bv]
                    [(procedure? fail)          (fail)]
                    [else                       fail])]
             [(eq? (vector-ref cs i) smr) (vector-ref (fbv-vals bv) i)]
             [else (loop (add1 i))])))])

;; The other half of the plumbing cost is that a component is itself invoked
;; through make-summary's variadic/spread/coerce wrapper -- so a bundle join
;; pays that once PER COMPONENT. Where a component's leaf and join are known
;; (they are private to summaries.rkt, so these are twins, not variants) the
;; bundle can call them directly. Registry keyed by the CANONICAL smr, so the
;; slot a guide projects by is unchanged.
(define fp-M (- (expt 2 61) 1))
(define fp-B 1000003)
(define (fp-leaf* s)
  (for/fold ([h 0] [sc 1] #:result (fp h sc)) ([c (in-string s)])
    (values (modulo (+ (* h fp-B) (char->integer c)) fp-M) (modulo (* sc fp-B) fp-M))))
(define (fp-join* x y)
  (fp (modulo (+ (* (fp-h x) (fp-scale y)) (fp-h y)) fp-M)
      (modulo (* (fp-scale x) (fp-scale y)) fp-M)))
(define (linecol-leaf* s)
  (define n (string-length s))
  (define-values (head lines last-nl)
    (for/fold ([head #f] [lines 0] [last-nl -1]) ([i (in-range n)])
      (if (char=? (string-ref s i) #\newline)
          (values (or head i) (add1 lines) i)
          (values head lines last-nl))))
  (linecol (or head n) lines (- n 1 last-nl)))
(define (linecol+* x y)
  (match-define (linecol xh xl xc) x)
  (match-define (linecol yh yl yc) y)
  (linecol (if (zero? xl) (+ xh yh) xh) (+ xl yl) (if (zero? yl) (+ xc yc) yc)))

(define fast-ops (make-hasheq))
(hash-set! fast-ops char-smr    (cons string-length  +))
(hash-set! fast-ops linecol-smr (cons linecol-leaf*  linecol+*))
(hash-set! fast-ops hash-smr    (cons fp-leaf*       fp-join*))

(define (fast-bundle . components)
  (define cs (list->vector components))
  (define n  (vector-length cs))
  (define leaves (for/vector ([c (in-list components)])
                   (car (hash-ref fast-ops c (lambda () (cons (lambda (s) (c s)) #f))))))
  (define joins  (for/vector ([c (in-list components)])
                   (or (cdr (hash-ref fast-ops c (lambda () (cons #f #f))))
                       (lambda (a b) (c a b)))))
  (define me (box #f))
  (define (owner) (unbox me))
  (define (leaf s) (fbv owner cs (build-vector n (lambda (i) ((vector-ref leaves i) s)))))
  (define id (leaf ""))
  (define (join a b)
    (define va (fbv-vals a)) (define vb (fbv-vals b))
    (fbv owner cs (build-vector n (lambda (i)
                                    ((vector-ref joins i) (vector-ref va i)
                                                          (vector-ref vb i))))))
  (define (co x)                                 ; rope-core's coerce, ordered by
    (cond [(fbv? x)           x]                 ;   frequency: values first
          [(rope? x)          (co (rope-summary x))]
          [(string? x)        (if (string=? x "") id (leaf x))]
          [(summary-part? x)  (part->summary x smr)]
          [else               x]))
  (define smr
    (case-lambda                                 ; the fast paths the variadic
      [()      id]                               ;   fold does not have
      [(x)     (co x)]
      [(x y)   (join (co x) (co y))]
      [(x y z) (join (join (co x) (co y)) (co z))]
      [args    (for/fold ([acc id]) ([x (in-list args)]) (join acc (co x)))]))
  (set-box! me smr)
  smr)

;; ---------- kw*: keywordize with no as-old bridge ----------
;; segment2's keywordize is (make-guide (guide-smr g) ((old:keywordize table)
;; (as-old g))) -- so judging one focus runs: the outer struct projects three
;; values, as-old FRAMES the inner guide with them (two bundle joins and a
;; struct-copy), and the inner struct projects the same three values back.
;; The two guides share a lens, so all of that is the identity. kw* calls the
;; inner guide's FN directly on the values it was already handed.
(define tok-M (- (expt 2 61) 1))                 ; segment.rkt's tok-join is private
(define (tok-join x y)                           ;   there; these four lines are its
  (fp (modulo (+ (* (fp-h x) (fp-scale y)) (fp-h y)) tok-M)   ; twin, not a variant
      (modulo (* (fp-scale x) (fp-scale y)) tok-M)))

(define ((kw* table) g)
  (define inner (guide-fn g))
  (make-guide (guide-smr g)
    (lambda (bs fs as)
      (match (inner bs fs as)
        [(list 'code (and spine (cons 0 (cons _ _))))
         (define b (old:atomhash-smr bs))
         (define f (old:atomhash-smr fs))
         (define whole
           (tok-join (old:ah-trail b)
                     (if (old:ah-all? f)
                         (tok-join (old:ah-lead f) (old:ah-lead (old:atomhash-smr as)))
                         (old:ah-lead f))))
         (if (and (eq? (old:ah-head f) 'atom) (hash-ref table whole #f))
             (list 'kw spine)
             (list 'code spine))]
        [r r]))))

;; ---------- lisp-run-guide*: the same guide, lazier ----------
;; segment.rkt's code branch binds s1, s2 and hd in one let, then tests
;;   (or (= 1 (char-smr fs)) (and (equal? s1 s2) (not (memq hd ...))))
;; -- so a single-char focus pays a second lisp-spines and a second lisp join
;; (for s2) and a forced lazy arm value (for hd) before discarding both. Only
;; s1 is needed unconditionally: it is the tag.
(define (run-index* b-arm cl m0)
  (match-define (arm _ n _ tailc pend _) b-arm)
  (+ n (if (boundary? tailc cl (if pend 'code m0)) 1 0)))

(define (lisp-run-guide* bs fs as)
  (define b (lisp-smr bs)) (define f (lisp-smr fs)) (define a (lisp-smr as))
  (define b-arm (aref b 0))
  (define m0 (arm-exit b-arm))
  (match-define (arm m1 n fc tailc pend _) (aref f (mode->i m0)))
  (define (after) (or (arm-first (aref a (mode->i m1))) 'code))
  (define cl
    (cond [(not fc)       #f]
          [(> n 0)        #f]
          [(eq? fc 'hash) (after)]
          [pend (if (boundary? tailc (after) 'code) #f fc)]
          [else fc]))
  (define (front-spine L R round)
    (define-values (fr _) (lisp-spines L R))
    (cons (round (car fr)) (cdr fr)))
  (and cl
       (case cl
         [(code)
          (define s1 (front-spine b (lisp-smr f a) floor))
          (and (or (= 1 (char-smr fs))                    ; the escape hatch first:
                   (and (equal? s1 (front-spine (lisp-smr b f) a floor))   ; s2 lazily
                        (not (memq (sexp-head (force (arm-val (aref f (mode->i m0)))))
                                   '(open close)))))                       ; hd lazily
               (list 'code s1))]
         [(string)
          (list 'string (front-spine b (lisp-smr f a)
                                     (if (memq m0 '(string escape)) floor ceiling)))]
         [(charlit)
          (list 'charlit (front-spine b (lisp-smr f a) ceiling))]
         [else (list cl (run-index* b-arm cl m0))])))

;; ---------- the same bundle, one knob per change ----------
;; fast-bundle changes three independent things at once. To attribute the
;; speedup, build it with each switchable:
;;   vec?  the VALUE: a vector (fbv) vs summaries.rkt's per-join #hasheq
;;   app?  the APPLICATION: a case-lambda with 1/2/3-arg cases vs
;;         make-summary's variadic/spread/coerce fold
;;   ops?  the COMPONENTS: their raw leaf/join where known vs calling each
;;         component as an smr, (c a b), so it coerces and projects itself
;; ops? = #f reproduces the original faithfully -- the component is handed the
;; WHOLE bundle values and does its own projection, which is where the extra
;; coerce/part->summary pair per component per join comes from.
(define (tunable-bundle vec? app? ops? components)
  (define cs (list->vector components))
  (define n  (vector-length cs))
  (define nm (hasheq))
  (define me (box #f))
  (define (owner) (unbox me))
  (define raw-leaf
    (for/vector ([c (in-list components)])
      (or (and ops? (car (hash-ref fast-ops c (lambda () (cons #f #f)))))
          (lambda (s) (c s)))))
  (define raw-join
    (for/vector ([c (in-list components)])
      (or (and ops? (cdr (hash-ref fast-ops c (lambda () (cons #f #f)))))
          #f)))
  (define (mk vals)
    (if vec?
        (fbv owner cs vals)
        (bundle-val owner
                    (for/hasheq ([c (in-vector cs)] [v (in-vector vals)]) (values c v))
                    nm)))
  (define (slot v i)
    (if vec? (vector-ref (fbv-vals v) i) (hash-ref (bundle-val-slots v) (vector-ref cs i))))
  (define (comp-join i a b)                      ; a, b: the whole bundle values
    (define rj (vector-ref raw-join i))
    (if rj
        (rj (slot a i) (slot b i))
        ((vector-ref cs i) a b)))                ; the original: c coerces + projects
  (define (leaf s) (mk (build-vector n (lambda (i) ((vector-ref raw-leaf i) s)))))
  (define id (leaf ""))
  (define (join a b) (mk (build-vector n (lambda (i) (comp-join i a b)))))
  (define (co x)
    (cond [(or (fbv? x) (bundle-val? x)) x]
          [(rope? x)                     (co (rope-summary x))]
          [(string? x)                   (if (string=? x "") id (leaf x))]
          [(summary-part? x)             (part->summary x smr)]
          [else                          x]))
  (define smr
    (case app?
      ;; the original: variadic over (spread join coerce coerce), rope-core's coerce
      [(#f)      (make-summary leaf join)]
      ;; same shape, but MY coerce -- isolates rope-core's coerce from spread
      [(spread)  (variadic (spread join co co) id)]
      ;; variadic kept, spread's call-with-values machinery dropped
      [(inline)  (variadic (lambda (a b) (join (co a) (co b))) id)]
      ;; ... and the 1-argument case no longer joins against the identity
      [else      (case-lambda
                   [()      id]
                   [(x)     (co x)]
                   [(x y)   (join (co x) (co y))]
                   [(x y z) (join (join (co x) (co y)) (co z))]
                   [args    (for/fold ([acc id]) ([x (in-list args)]) (join acc (co x)))])]))
  (set-box! me smr)
  smr)

;; ============================================================================
(module+ main
  (define tbl (old:keyword-table "define" "lambda" "let" "if" "cond"))
  (define text
    (apply string-append
           (for/list ([i (in-range 160)])       ; 800 rows
             (format (string-append "(define (f~a x)        ; helper ~a\n"
                                    "  (if (< x ~a)\n      (g \"s~a\" x)\n"
                                    "      (h ~a (f~a (- x 1)))))\n\n") i i i i i i))))
  (define TOP 100) (define H 80) (define REPS 10)

  ;; a variant = a bundle constructor x a guide constructor
  (define (paint make-B make-G #:min-chars [mc 4])
    (define Bv   (make-B))
    (define R    (make-rope Bv))
    (define doc  (R text))
    (define G    (make-G Bv))
    (define vp   (viewport TOP (+ TOP H -1) G))
    (define steps (box 0))
    (define k0   (viewport-key mc))
    (define (run)                                 ; ONE cold paint
      (define seg (segment R #:key (lambda (t g) (set-box! steps (add1 (unbox steps)))
                                           (k0 t g))
                             #:cap 1000000))
      (values (seg doc vp) (memo-size seg)))
    (define-values (segs0 entries) (run))         ; once, warm the code paths
    (set-box! steps 0)
    (collect-garbage)
    (define t0 (current-inexact-milliseconds))
    (for ([_ (in-range REPS)]) (call-with-values run void))
    (define ms (/ (- (current-inexact-milliseconds) t0) REPS))
    (values ms (quotient (unbox steps) REPS) entries segs0))

  (define (view segs) (map (lambda (s) (cons (car s) (format "~a" (cdr s)))) segs))

  (define (B5)  (bundle      char-smr linecol-smr lisp-smr old:atomhash-smr hash-smr))
  (define (F5)  (fast-bundle char-smr linecol-smr lisp-smr old:atomhash-smr hash-smr))
  (define (B4)  (bundle      char-smr linecol-smr lisp-smr hash-smr))          ; no keywords
  (define (F4)  (fast-bundle char-smr linecol-smr lisp-smr hash-smr))

  (define (G-base  Bv) ((keywordize tbl) (lisp-run-guide Bv)))    ; segment2's, via as-old
  (define (G-kw*   Bv) ((kw* tbl) (lisp-run-guide Bv)))           ; native framed keywordize
  (define (G-lisp* Bv) ((keywordize tbl) (make-guide Bv lisp-run-guide*)))
  (define (G-both  Bv) ((kw* tbl) (make-guide Bv lisp-run-guide*)))
  (define (G-nokw  Bv) (lisp-run-guide Bv))                       ; no keywords at all

  (define base-out #f)
  (define (row label make-B make-G #:min-chars [mc 4] #:check [check #t])
    (define-values (ms steps entries segs) (paint make-B make-G #:min-chars mc))
    (unless base-out (set! base-out (view segs)))
    (printf "  ~a ~a ~a ~a ~a ~a~n"
            (~a label #:min-width 40)
            (~a (~r ms #:precision 2) #:min-width 9)
            (~a (~r (* 1000 (/ ms (max 1 steps))) #:precision 1) #:min-width 9)
            (~a steps #:min-width 8) (~a entries #:min-width 8)
            (cond [(not check) ""]
                  [(equal? (view segs) base-out) "same"]
                  [else "DIFFERENT"])))

  (printf "cold paint, ~a-row window on an 800-row document, mean of ~a~n~n" H REPS)
  (printf "  ~a ~a ~a ~a ~a ~a~n"
          (~a "variant" #:min-width 40) (~a "ms" #:min-width 9)
          (~a "us/step" #:min-width 9) (~a "steps" #:min-width 8)
          (~a "entries" #:min-width 8) "tags")

  (printf "~n-- baseline --~n")
  (row "bundle + keywordize(as-old) + lisp"      B5 G-base)

  (printf "~n-- one change at a time --~n")
  (row "FAST-BUNDLE"                             F5 G-base)
  (row "kw* (no as-old frame)"                   B5 G-kw*)
  (row "lisp-run-guide* (lazy s2/hd)"            B5 G-lisp*)

  (printf "~n-- combined --~n")
  (row "kw* + lisp*"                             B5 G-both)
  (row "FAST-BUNDLE + kw*"                       F5 G-kw*)
  (row "FAST-BUNDLE + kw* + lisp*"               F5 G-both)

  (printf "~n-- what the keyword layer costs (different tags, by design) --~n")
  (row "no keywords, plain bundle"               B4 G-nokw #:check #f)
  (row "no keywords, FAST-BUNDLE"                F4 G-nokw #:check #f)

  (printf "~n-- min-chars sweep (viewport-key's refusal threshold) --~n")
  (for ([mc (in-list '(1 2 4 8 16 32))])
    (row (format "FAST-BUNDLE + kw* + lisp*, min-chars ~a" mc) F5 G-both #:min-chars mc))

  ;; which of fast-bundle's three changes actually pays?
  (printf "~n-- fast-bundle, one change at a time (vec / app / ops) --~n")
  (define (TB vec? app? ops?)
    (lambda () (tunable-bundle vec? app? ops?
                               (list char-smr linecol-smr lisp-smr old:atomhash-smr hash-smr))))
  (row "none (rebuilt original)"                 (TB #f #f #f) G-base)
  (row "vec only   (vector value, no hasheq)"    (TB #t #f #f) G-base)
  (row "app only   (case-lambda application)"    (TB #f #t #f) G-base)
  (row "ops only   (raw component leaf/join)"    (TB #f #f #t) G-base)
  (row "vec + app"                               (TB #t #t #f) G-base)
  (row "vec + ops"                               (TB #t #f #t) G-base)
  (row "app + ops"                               (TB #f #t #t) G-base)
  (row "all three  (= fast-bundle)"              (TB #t #t #t) G-base)

  ;; variadic is ALREADY a case-lambda whose first case is (op a b). So what
  ;; does "app" actually buy? Three candidates hide in make-summary's
  ;; (variadic (spread combine coerce coerce) id): rope-core's coerce, spread's
  ;; call-with-values machinery, and variadic's 1-ARY case (op id a) -- which
  ;; makes the single-argument (smr x), the walk's commonest call, do a full
  ;; join against the identity.
  (printf "~n-- inside \"app\": what make-summary's application actually costs --~n")
  (row "make-summary (variadic+spread+coerce)"   (TB #t #f     #t) G-base)
  (row "  ... same, but my coerce"               (TB #t 'spread #t) G-base)
  (row "  ... and no spread (variadic kept)"     (TB #t 'inline #t) G-base)
  (row "  ... and no (op id a) on 1 argument"    (TB #t #t     #t) G-base)

  ;; the fast leaf/join twins must agree with summaries.rkt's originals
  (printf "~n-- fast component ops agree with summaries.rkt --~n")
  (let ([corpus (list "" " " "a" "ab cd" "a\nb\n" "\n\n" "  ab  " "x\ny z\nw"
                      "(define (f x)\n  (g \"s\" x))\n")])
    (printf "  leaves ~a   joins ~a~n"
            (if (for/and ([s corpus])
                  (and (equal? (linecol-leaf* s) (linecol-smr s))
                       (equal? (fp-leaf* s) (hash-smr s))))
                'agree 'DIFFER)
            (if (for*/and ([a corpus] [b corpus])
                  (and (equal? (linecol+* (linecol-smr a) (linecol-smr b)) (linecol-smr a b))
                       (equal? (fp-join* (hash-smr a) (hash-smr b)) (hash-smr a b))))
                'agree 'DIFFER)))

  ;; min-chars trades cold-paint cost against SCROLL reuse -- measure both
  (printf "~n-- min-chars: cold paint vs one-row scroll (best variant) --~n")
  (printf "  ~a ~a ~a ~a ~a~n"
          (~a "min-chars" #:min-width 12) (~a "cold ms" #:min-width 9)
          (~a "entries" #:min-width 9) (~a "scroll ms/row" #:min-width 14)
          (~a "misses/row" #:min-width 10))
  (for ([mc (in-list '(2 4 8 16 32))])
    (define Bv (F5)) (define R (make-rope Bv)) (define doc (R text))
    (define G (G-both Bv))
    (define seg (segment R #:key (viewport-key mc) #:cap 1000000))
    (collect-garbage)
    (define t0 (current-inexact-milliseconds))
    (void (seg doc (viewport TOP (+ TOP H -1) G)))          ; cold paint
    (define cold (- (current-inexact-milliseconds) t0))
    (define e0 (memo-size seg))
    (define t1 (current-inexact-milliseconds))
    (for ([i (in-range 1 41)])                              ; 40 one-row scrolls
      (void (seg doc (viewport (+ TOP i) (+ TOP i H -1) G))))
    (define scroll (/ (- (current-inexact-milliseconds) t1) 40))
    (printf "  ~a ~a ~a ~a ~a~n"
            (~a mc #:min-width 12) (~a (~r cold #:precision 2) #:min-width 9)
            (~a e0 #:min-width 9) (~a (~r scroll #:precision 3) #:min-width 14)
            (~a (quotient (- (memo-size seg) e0) 40) #:min-width 10)))

  ;; Re-profiling the OPTIMIZED variant puts regexp-match* on top at 11.1%
  ;; self. It is sexp-summary.rkt's sexp-leaf tokenizer -- run on every leaf,
  ;; and split re-runs it on both halves at every descent into a leaf. It
  ;; allocates a list of substrings only to read each one's first char. A
  ;; char scan produces the same list with no allocation; sexp-leaf is private
  ;; to a stable copied file, so this only SIZES the remaining win.
  (printf "~n-- what is left: sexp-leaf's regexp tokenizer vs a char scan --~n")
  (let* ([px #px"[][(){}]|[^][(){}\\s]+"]
         [delim? (lambda (c) (or (char-whitespace? c) (memv c '(#\( #\) #\[ #\] #\{ #\}))))]
         [brack? (lambda (c) (memv c '(#\( #\) #\[ #\] #\{ #\})))]
         [by-rx (lambda (s) (for/list ([t (in-list (regexp-match* px s))]) (string-ref t 0)))]
         [by-scan (lambda (s)                       ; first char of each bracket / atom run
                    (define n (string-length s))
                    (let loop ([i 0] [acc '()])
                      (cond [(= i n) (reverse acc)]
                            [(brack? (string-ref s i)) (loop (add1 i) (cons (string-ref s i) acc))]
                            [(char-whitespace? (string-ref s i)) (loop (add1 i) acc)]
                            [else (let run ([j (add1 i)])
                                    (if (or (= j n) (delim? (string-ref s j)))
                                        (loop j (cons (string-ref s i) acc))
                                        (run (add1 j))))])))]
         [leaves (for/list ([k (in-range 0 (- (string-length text) 32) 32)])
                   (substring text k (+ k 32)))]     ; rope-core's max-leaf is 32
         [ok (for/and ([s (in-list leaves)]) (equal? (by-rx s) (by-scan s)))]
         [t (lambda (f) (collect-garbage)
                    (define t0 (current-inexact-milliseconds))
                    (for ([_ (in-range 20)]) (for ([s (in-list leaves)]) (void (f s))))
                    (- (current-inexact-milliseconds) t0))]
         [ms-rx (t by-rx)] [ms-sc (t by-scan)])
    (printf "  same tokens: ~a   regexp ~a ms   char scan ~a ms   ratio ~a x~n"
            (if ok 'yes 'NO) (~r ms-rx #:precision 1) (~r ms-sc #:precision 1)
            (~r (/ ms-rx (max 0.001 ms-sc)) #:precision 1))
    (printf "  it is 11.1% of the optimized paint, so replacing it would take~n")
    (printf "  ~a ms to about ~a ms~n" 24 (~r (* 24 (- 1 (* 0.111 (- 1 (/ ms-sc ms-rx)))))
                                              #:precision 1)))

  (printf "~n-- and what a WARM frame costs under the best variant --~n")
  (let* ([Bv (F5)] [R (make-rope Bv)] [doc (R text)] [G (G-both Bv)]
         [vp (viewport TOP (+ TOP H -1) G)]
         [seg (segment R #:key (viewport-key) #:cap 1000000)]
         [p   1000])
    (void (seg doc vp))
    (define plain (segment R #:key no-memo))
    (define (frame) ((refine-at R char-smr (list p (add1 p))
                                #:tag (lambda (t i) (list t i)))
                     (seg doc vp)))
    (void (frame))
    (collect-garbage)
    (define t0 (current-inexact-milliseconds))
    (for ([_ (in-range 200)]) (void (frame)))
    (printf "  warm repaint + refine-at: ~a ms/frame~n"
            (~r (/ (- (current-inexact-milliseconds) t0) 200) #:precision 3))))
