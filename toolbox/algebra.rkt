#lang racket

;; Small algebraic helpers -- retracts/isos (spl/iso) and a few value/list
;; combinators -- each documented at its definition (the canonical home other files
;; point to; the provide list is the surface map). Intended as a reusable helper
;; library, so some surface is built out past what this project strictly needs.
;; The store-shaped `opt` optic that once lived here was RETIRED 2026-07-12, its
;; consumers moved to toolbox/stage.rkt's staged optic; the whole store-shaped
;; generation is snapshot in toolbox/old/algebra-store-shaped.rkt (the record-opt one
;; before it in toolbox/old too, the van Laarhoven one in deprecated/deprecated-7).
;; Narrative -- the iso group law, the inlining rationale -- in scribble/algebra.scrbl.

(provide (struct-out spl)       ; (spl to from): a SPLITTING -- section `to` (embeds),
                                 ;   retraction `from` (may lose), law (from . to) = id;
                                 ;   callable = applies `to`
         compose-spl             ; compose any number of spls; froms reversed
         spl-normalize           ; e = to . from -- the idempotent this pair SPLITS
         spl-law? check-spl-laws ; the one-sided round trip; the inputs that fail it
         iso iso? iso-to iso-from ; (iso to from): a spl with the SECOND law added --
                                 ;   substruct, so every spl op takes an iso unchanged
         inverse-iso             ; the (from, to) swap -- iso-ONLY (a mere retract can't flip)
         compose-iso             ; compose any number of isos; inverses reversed
         expt-iso                ; integer powers of an iso (scmutils function arithmetic)
         iso-law? check-iso-laws ; = the spl law (from . to), kept under its old name
         ;; The store-shaped `opt` family (opt / opt-get / ... / list-of / opt-list / focal /
         ;; varg / vdiag) was retired 2026-07-12 -- succeeded by toolbox/stage.rkt's staged
         ;; optic; the whole store-shaped generation is snapshot in toolbox/old/algebra-store-shaped.rkt.
         pure                    ; (pure v ...): the constant fn, ignoring its args and returning the v ... as values (K)
         on                      ; (on op f) a ... = (op (f a) ...) -- Haskell's `on`
         arg                     ; project args by 0-based position
         pass                    ; the thrush: ((pass . args) f) = (apply f args), values-transparent
         fork                    ; apply each f to the same arg(s), as values -- the fan-out
         spread                  ; apply each fn to its own arg, combine with h --
                                 ;   stream-wise: multi-value fns combine COLUMNWISE
         parallel                ; apply each fn to the WHOLE arg tuple, combine with h --
                                 ;   spread's broadcast twin, stream-wise the same way
         variadic                ; lift a binary op + seed to a variadic left fold
         fixed                   ; iterate to a fixed point
         scanl scanr             ; every intermediate fold value, seed included (length n+1)
         lexicographic)          ; first-difference 3-way order on sequences

;; ---------- spl / iso ----------
;; spl: a SPLITTING -- the focused (to, from) pair, `to` the section (embeds, loses
;; nothing), `from` the retraction (projects, may lose), with the ONE-SIDED law
;; (from . to) = id. The name is the categorical one: such a pair is exactly a
;; splitting of the idempotent e = to . from (spl-normalize) -- every pair with the
;; law splits its own e, and every split idempotent yields such a pair.
;; prop:procedure runs `to`, so a spl is callable as its forward function -- only
;; its own combinators see the other half. An iso is a spl with the second law
;; (to . from) = id added, as a SUBSTRUCT: every spl combinator and battery takes
;; an iso unchanged; `iso?` gates the ops needing the second law (inverse-iso,
;; expt-iso). The laws live in the batteries, not the constructor -- same
;; compromise as the summary battery.
(struct spl (to from)
  #:property prop:procedure (struct-field-index to))

(struct iso spl ())

;; the accessors live on the parent; the iso names kept as aliases
(define iso-to   spl-to)
(define iso-from spl-from)

;; iso-ONLY: swapping a mere spl points its law the wrong way.
(define (inverse-iso i) (iso (iso-from i) (iso-to i)))

;; compose: to's compose in order, from's in reverse -- closed on spls;
;; compose-iso is the same fold closed on isos. () = the identity.
(define (compose-spl . rs)
  (spl (apply compose (map spl-to rs))
       (apply compose (map spl-from (reverse rs)))))
(define (compose-iso . is)
  (iso (apply compose (map spl-to is))
       (apply compose (map spl-from (reverse is)))))

;; Integer powers of an iso, in the style of scmutils function arithmetic: n<0 uses
;; the inverse, so (expt-iso i -1) = (inverse-iso i). Closed on isos.
(define (expt-iso i n)
  (cond [(negative? n) (expt-iso (inverse-iso i) (- n))]
        [else (for/fold ([acc (iso values values)]) ([_ (in-range n)]) (compose-iso i acc))]))

;; normalize: the split idempotent e = to . from -- everything the spl forgets,
;; as a map. Idempotent; the identity exactly on the section's image (and there
;; the pair is an iso).
(define (spl-normalize r) (compose (spl-to r) (spl-from r)))

;; the law: x round-trips (from . to) unchanged -- sampled on the section's side.
;; This is the RETRACT law; iso-law?/check-iso-laws keep their historical names
;; for it (they only ever checked this side -- the second iso law is the pair
;; (to . from), checkable via (spl-law? (inverse-iso i)) on the other side).
(define (spl-law? r x) (equal? ((spl-from r) ((spl-to r) x)) x))
(define (check-spl-laws r xs) (filter (lambda (x) (not (spl-law? r x))) xs))
(define iso-law? spl-law?)
(define check-iso-laws check-spl-laws)

;; pure: the constant function, variadic in its result -- (pure v ...) ignores its
;; arguments and returns the v ... as values. The K combinator, lifting plain values
;; into a transform that disregards its input (e.g. a re-edge that just installs v).
(define ((pure . vs) . _) (apply values vs))

;; on: (on op f) a b ... = (op (f a) (f b) ...) -- the n-ary Haskell `on`. E.g.
;; (on guide smr) reads each side of a guide through a summary.
(define ((on op f) . args) (apply op (map f args)))

;; arg: ((arg i j ...) . xs) returns the i-th, j-th, ... arguments as multiple values.
(define ((arg . is) . xs)
  (let ([v (list->vector (take xs (add1 (apply max is))))])
    (apply values (map (lambda (i) (vector-ref v i)) is))))

;; pass: the thrush -- hold a tuple of args, then feed them to ONE function, its
;; values passing through untouched: ((pass . args) f) = (apply f args). The
;; args-first FAN-OUT is fork with the calls flipped -- ((pass . args) (fork f g ...))
;; -- so pass stays fully values-transparent where fork's fan-out cannot.
(define ((pass . args) f) (apply f args))

;; fork: THE fan-out -- hold the functions, then apply each to the same args, as
;; values: ((fork f g ...) . args) = (values (apply f args) (apply g args) ...); each
;; f single-valued (the values-stream carries one slot per f). ((fork f g) x) =
;; (values (f x) (g x)), e.g. (fork read values) reads and passes its arg through.
(define ((fork . fs) . args)
  (apply values (map (lambda (f) (apply f args)) fs)))

;; spread: each function to its corresponding argument, results combined by `h` --
;; ((spread h f g ...) a b ...) = (h (f a) (g b) ...) -- and STREAM-WISE: an f may
;; return several values, and h then combines the streams COLUMNWISE, each position
;; individually: with (f a) = (values p q) and (g b) = (values r s), the result is
;; (values (h p r) (h q s)). Single-value fs recover the plain shape; stream lengths
;; must agree (map's error at the seam, not a silent drop). So (spread list v w) is
;; the TRANSPOSE of two viewers into parallel lists -- view-join's zip twin. The
;; 2-ary case (the make-summary hot path, (spread combine coerce coerce)) keeps an
;; allocation-free fast path when both fs are single-valued.
(define (value-stream f x) (call-with-values (lambda () (f x)) list))
(define spread
  (case-lambda
    [(h f)   (lambda (a)
               (call-with-values (lambda () (f a))
                 (case-lambda [(p) (h p)]
                              [ps  (apply values (map h ps))])))]
    [(h f g) (lambda (a b)
               (call-with-values (lambda () (f a))
                 (case-lambda
                   [(p) (call-with-values (lambda () (g b))
                          (case-lambda [(q) (h p q)]                            ; the fast path
                                       [qs  (apply values (map h (list p) qs))]))]
                   [ps  (apply values (map h ps (value-stream g b)))])))]
    [(h . fs)  (lambda xs (apply values (apply map h (map value-stream fs xs))))]))

;; parallel: every function fed the WHOLE argument tuple, results combined by `h` --
;; spread's broadcast twin (= (compose h (fork f g ...)), named; SDF's
;; parallel-combine): ((parallel h f g) . args) = (h (f . args) (g . args)).
;; Stream-wise like spread -- a multi-value f contributes a stream, h combining the
;; streams COLUMNWISE -- so (parallel list v w) zips same-world viewers into
;; parallel lists: ((parallel list (edge-view 0) (edge-view 1)) z) = (values Ls Rs).
(define ((parallel h . fs) . args)
  (define streams (map (lambda (f) (call-with-values (lambda () (apply f args)) list)) fs))
  (if (andmap (lambda (s) (null? (cdr s))) streams)
      (apply h (map car streams))                     ; all single-valued: plain combine
      (apply values (apply map h streams))))          ; else columnwise, each position its own h

;; variadic: lift a MONOID `op` (acc-first, (op acc x)) with unit `id` to any arity, left-
;; folding from the FIRST argument. `id` seeds only the empty call, so the 2-ary and n-ary
;; paths compute no (op id x) -- the wasted fuse a container-rebuilding op can't short out.
;; This ASSUMES the identity law (op id x) = x: folding from the first arg equals folding
;; from id only for a monoid, so a non-monoid op no longer sees id folded in (that was the
;; old contract). The lone 1-ary case keeps (op id a), letting a coercing op preprocess a
;; single argument. make-summary depends on this; the law is battery-checked per summary.
(define (variadic op id)
  (case-lambda
    [(a b)      (op a b)]
    [(a)        (op id a)]
    [()         id]
    [(a . rest) (foldl (lambda (x acc) (op acc x)) a rest)]))

;; fixed: iterate `improve` from a seed to a fixed point; seed and improve may carry
;; several values. Halt is `same?` over `(key v ...)` applied to the tuple as args
;; (default key=list -> whole-tuple equal?), mirroring remove-duplicates' [same?] #:key.
;; An improve that no-ops at the fixpoint needs no stop test. Arities 1..4 inlined; 5+
;; falls to `rest-loop`. Inlining rationale: scribble.
(define (fixed improve [same? equal?] [key list])
  ;; macro so its template captures improve/same?/key from this scope
  (define-syntax (fixed-case stx)
    (syntax-case stx ()
      [(_ v ...)
       (with-syntax ([(v* ...) (generate-temporaries #'(v ...))])
         #'(let loop ([v v] ... [kp (key v ...)])
             (define-values (v* ...) (improve v ...))
             (define kp* (key v* ...))
             (if (same? kp kp*) (values v* ...) (loop v* ... kp*))))]))
  ;; the generic tail: tuple held as a list, for any arity past the inlined ones
  (define (rest-loop xs)
    (let ([step (compose list improve)])
      (let loop ([xs xs] [kp (apply key xs)])
        (define ys (apply step xs))
        (define kp* (apply key ys))
        (if (same? kp kp*) (apply values ys) (loop ys kp*)))))
  (case-lambda
    [(a)       (fixed-case a)]
    [(a b)     (fixed-case a b)]
    [(a b c)   (fixed-case a b c)]
    [(a b c d) (fixed-case a b c d)]
    [xs        (rest-loop xs)]))

;; scanl / scanr: every intermediate value of the corresponding fold, seed included --
;; length n+1, the seed at its own end. One pass each, allocating exactly the result
;; cells: scanl emits the running prefix as it recurses ((f acc x), acc-first, as
;; foldl-shaped accumulation reads); scanr conses (f x suffix) onto the scan of the
;; rest, whose head IS the running suffix.
(define (scanl f z xs)
  (cons z (if (null? xs) '() (scanl f (f z (car xs)) (cdr xs)))))

(define (scanr f z xs)
  (foldr (lambda (x acc) (cons (f x (car acc)) acc)) (list z) xs))

;; lexicographic: lift an element comparison `cmp` (-> {-1,0,1}) to a 3-way order on
;; sequences -- first non-zero verdict decides; a prefix precedes its extension.
(define ((lexicographic cmp) xs ys)
  (let loop ([xs xs] [ys ys])
    (cond [(null? xs) (if (null? ys) 0 -1)]
          [(null? ys) 1]
          [else (let ([v (cmp (car xs) (car ys))])
                  (if (zero? v) (loop (cdr xs) (cdr ys)) v))])))

;; ============================================================================
;; WIP -- not yet load-bearing; surface and semantics may still move.
;; ============================================================================

(provide lockstep             ; (lockstep f g ...): N equivalent fns worn as one self-checking procedure
         lockstep?            ; recognizes one
         lockstep-on          ; (lockstep-on x): checking-on sibling; non-locksteps pass through
         lockstep-off         ; (lockstep-off x): run only the trusted impl, raw; non-locksteps pass through
         lockstep-mode)       ; 'on | 'off

;; lockstep: bundle N functions that should compute the same thing, worn as one procedure.
;; Born ON: every call runs all impls and checks they agree before returning the common
;; result. Each impl's return is captured as a value tuple (so multiple-values impls work),
;; and agreement is checked per value-position: a value column must be equal? across impls;
;; a column where every impl returns a procedure isn't comparable yet, so its check rides
;; down to the next application (a re-bundled lockstep in that slot); a procedure-vs-value
;; split in a column, or differing tuple arities, is a disagreement. `lockstep-off` flips an
;; instance to OFF: run only the trusted impl, raw -- one run, results unwrapped (a returned
;; procedure comes back plain, no deferral). Trusted defaults to the LAST impl (we list the
;; ordinary form first, the one to run when off last); `#:trusted i` overrides. Sound only
;; for pure, deterministic fns -- N runs per call. The struct `steps` is private.
(struct steps (fs mode trusted)
  #:property prop:procedure
  (lambda (self . args)
    (case (steps-mode self)
      [(off) (apply (list-ref (steps-fs self) (steps-trusted self)) args)]   ; one run, raw
      [else
       (define rss (map (lambda (f) (call-with-values (lambda () (apply f args)) list))
                        (steps-fs self)))             ; one value tuple per impl
       (define n (length (car rss)))
       (unless (andmap (lambda (vs) (= (length vs) n)) rss)
         (error 'lockstep "arity mismatch: ~e" rss))
       (apply values
        (for/list ([j (in-range n)])                  ; resolve column by column
          (define col (map (lambda (vs) (list-ref vs j)) rss))
          (cond
            [(andmap procedure? col) (steps col 'on (steps-trusted self))]   ; defer to next apply
            [(ormap procedure? col) (error 'lockstep "shape mismatch at value ~a: ~e" j col)]
            [else
             (for ([r (in-list (cdr col))] [i (in-naturals 1)])
               (unless (equal? r (car col))
                 (error 'lockstep "impl ~a fell out of step: ~e vs ~e" i r (car col))))
             (car col)])))])))

(define (lockstep #:trusted [t #f] . fs) (steps fs 'on (or t (sub1 (length fs)))))
(define lockstep? steps?)
(define lockstep-mode steps-mode)

;; lockstep-on / -off are universal: flip a lockstep's mode, but pass any other value
;; through untouched -- so arbitrary functions can be wrapped and simply no-op. This also
;; lets you silence one deferred sub-stage: in ON mode each stage is itself a lockstep.
(define (lockstep-on  x) (if (steps? x) (steps (steps-fs x) 'on  (steps-trusted x)) x))
(define (lockstep-off x) (if (steps? x) (steps (steps-fs x) 'off (steps-trusted x)) x))

;; ========== EXPERIMENTAL: curried lambda =======================================
;; Provisional, opt-in: (require (submod "algebra.rkt" experimental)).
;; Shadows `lambda` so a parenthesised binder HEAD desugars to a curried lambda,
;; one level per nesting, to any depth:
;;   (lambda ((x w) y) e)   = (lambda (x w) (lambda (y) e))     ; multi-arg stages
;;   (lambda (((a) b) c) e) = (lambda (a) (lambda (b) (lambda (c) e)))
;; A flat binder -- or rest / keyword / optional args -- is the ordinary lambda,
;; passed through untouched; only a parenthesised head triggers currying. The
;; binder reads like the call site: ((L R) focus) abstracts as
;; (lambda (L R) (lambda (focus) e)) -- exactly an ilens rebaser's shape.
;; `lambda*` reads its whole binder as a SPINE OF STAGES, curried left to right -- one
;; application per stage. A stage is either a bare identifier or a parenthesised group:
;;   - bare x            -> a rest stage (%lambda x ...): binds the whole arg LIST of its
;;                          application;
;;   - group (x1 ... xn) -> a fixed stage (%lambda (x1 ... xn) ...): binds the args
;;                          individually; nest one level -- (x) -- for a single arg, and
;;                          the empty group () expects a zero-arg application.
;; The spine is a proper list; the empty spine is no stages at all -- just the body.
;;   (lambda* (a b c d) e)             = (lambda a (lambda b (lambda c (lambda d e))))
;;   (lambda* ((a1 a2) b (c1 c2) d) e) = (lambda (a1 a2) (lambda b (lambda (c1 c2) (lambda d e))))
;;   (lambda* (a ()) e)                = (lambda a (lambda () e))
;; Eliminate with staged-apply -- one list per stage: a bare stage captures its list, a
;; group stage spreads it. Contrast `lambda` above, which peels only a parenthesised
;; head and dumps the tail into one flat binder. The `*` is the let*/for* sense -- a
;; clause list nested into single-clause (here curried) forms -- NOT match*'s sense.
;; Self-contained -- delete this submodule to retract.
(module+ experimental
  (require (only-in racket/base [lambda %lambda]))    ; the genuine lambda, renamed
  (provide lambda lambda*)

  (define-syntax lambda
    (syntax-rules ()
      [(_ ((h . inner) . args) body ...)              ; parenthesised head -> peel one level
       (lambda (h . inner) (%lambda args body ...))]
      [(_ formals body ...)                           ; flat / rest / kw / optional -> real lambda
       (%lambda formals body ...)]))

  (define-syntax lambda*                              ; a spine of curried stages, left to right
    (syntax-rules ()
      [(_ ((f ...))        body ...) (%lambda (f ...) body ...)]                 ; group stage, last  (() = nullary)
      [(_ ((f ...) . rest) body ...) (%lambda (f ...) (lambda* rest body ...))]  ; group stage, more
      [(_ (x)              body ...) (%lambda x body ...)]                       ; bare stage, last   (x = the list)
      [(_ (x . rest)       body ...) (%lambda x (lambda* rest body ...))]        ; bare stage, more
      [(_ ()               body ...) (let () body ...)]))                        ; no stages -> just the body

  ;; staged-apply -- the curried binder's eliminator: one list per stage,
  ;;   (staged-apply f l1 l2 ...) = (apply (apply f l1) l2) ...
  ;; the final application is in TAIL position, so a stage's (values g put) propagates
  ;; (a foldl would force each step to one value and drop the second).
  (provide staged-apply)
  (define (staged-apply f . ls)
    (cond [(null? ls) f]
          [(null? (cdr ls)) (apply f (car ls))]
          [else (apply staged-apply (apply f (car ls)) (cdr ls))]))

  (module+ test
    (require rackunit)
    (check-equal? (staged-apply + '(1 2 3)) 6)                           ; one list = plain apply
    (check-equal? (staged-apply (lambda ((L R) focus) (list L R focus))  ; eliminates the curried binder
                                '(l r) '(f))
                  '(l r f))
    (check-equal? (staged-apply (lambda (((a) b) c) (list a b c)) '(1) '(2) '(3)) '(1 2 3))
    (check-eq?    (staged-apply car) car)                                ; no lists = the function itself

    (check-equal? (((lambda ((x w) y) (list x w y)) 1 2) 3) '(1 2 3))    ; multi-arg first stage
    (check-equal? ((((lambda (((a) b) c) (list a b c)) 1) 2) 3) '(1 2 3)); three stages, one arg each
    (check-equal? (((lambda ((L R) focus) (list L R focus)) 'l 'r) 'f)   ; the rebaser shape
                  '(l r f))
    (check-equal? ((lambda (a b) (+ a b)) 2 5) 7)                        ; flat = ordinary lambda

    ;; lambda* -- a spine of curried stages: a bare stage binds its whole arg LIST, a
    ;; group binds individually, () expects an empty application
    (check-equal? (staged-apply (lambda* (a b) (list a b)) '(1 2) '(3 4 5)) '((1 2) (3 4 5)))
    (check-equal? (staged-apply (lambda* ((a1 a2) b) (list a1 a2 b)) '(1 2) '(9)) '(1 2 (9)))
    (check-equal? ((lambda* ((x)) (* x x)) 6) 36)                        ; nest for a single arg
    (check-equal? (((lambda* (a ()) a) 1 2)) '(1 2))                     ; () = a zero-arg stage

    (check-equal? (apply (lambda xs xs) '(1 2 3)) '(1 2 3))              ; rest arg, untouched
    (check-equal? ((lambda (x [y 10]) (+ x y)) 5) 15)))                  ; optional arg, untouched

;; ========== EXPERIMENTAL: church-apply =========================================
;; Provisional, opt-in: (require (submod "algebra.rkt" experimental)).
;; apply's shape, but the result is CHURCH-ENCODED multiple values: call f on the
;; args and reify its (values ...) as a function awaiting a consumer k:
;;   ((church-apply f a ...) k) = (call-with-values (%lambda () (f a ...)) k)
;; (compose k f) threads f's values into k, so the definition is just apply.
;; Self-contained -- delete this submodule to retract.
(module+ experimental
  (provide church-apply)

  (define ((church-apply f . args) k)
    (apply (compose k f) args))

  (module+ test
    (require rackunit)
    (check-equal? ((church-apply values 1 2) list) '(1 2))              ; materialize
    (check-equal? ((church-apply quotient/remainder 17 5) list) '(3 2)) ; real multi-values
    (check-equal? ((church-apply values 1 2) (%lambda (a b) a)) 1)      ; consume: pick one
    (check-equal? ((church-apply add1 41) values) 42)))                 ; single value, plain

;; ============================================================================
(module+ test
  (require rackunit)

  (define inc (iso add1 sub1))

  ;; --- applying an iso runs its forward side; `inverse` runs the other ---
  (check-equal? (inc 10) 11)
  (check-equal? ((inverse-iso inc) 11) 10)

  ;; --- integer powers, closed on isos ---
  (check-equal? ((expt-iso inc 3) 10) 13)         ; forward thrice
  (check-equal? ((expt-iso inc -3) 13) 10)        ; negative = inverse's power
  (check-equal? ((expt-iso inc 0) 99) 99)         ; n = 0 is the identity iso

  ;; --- the result is still an iso: invert it, re-exponentiate it ---
  (check-equal? ((inverse-iso (expt-iso inc 3)) 13) 10)

  ;; --- compose-iso is variadic: any number of isos, inverses reversed ---
  (check-equal? ((compose-iso inc inc inc) 10) 13)          ; three composed, forward
  (check-equal? ((inverse-iso (compose-iso inc inc inc)) 13) 10)
  (check-equal? ((compose-iso) 42) 42)                      ; no isos = the identity iso

  ;; --- the identities that closure buys ---
  (define i (iso (lambda (x) (* 2 x)) (lambda (x) (/ x 2))))
  ;; inverse and power commute
  (check-equal? ((inverse-iso (expt-iso i 4)) 48)
                ((expt-iso i -4) 48))
  ;; expt -1 = inverse  (the generic-arithmetic identity, inside the type)
  (check-equal? ((expt-iso i -1) 6) ((inverse-iso i) 6))
  ;; (i^m)^n = i^(m*n)
  (check-equal? ((expt-iso (expt-iso i 2) 3) 5)
                ((expt-iso i 6) 5))

  ;; --- the iso law: a genuine iso round-trips, a non-iso is caught ---
  (check-true  (iso-law? inc 10))
  (check-true  (iso-law? (expt-iso inc 3) 10))
  (check-equal? (check-iso-laws inc '(0 5 -3 99)) '())
  (define bad (iso add1 add1))             ; from doesn't undo to
  (check-false (iso-law? bad 10))
  (check-equal? (check-iso-laws bad '(1 2 3)) '(1 2 3))

  ;; --- spl: iso's supertype -- one law, subsumption, normalize ---
  (define int<-real (spl exact->inexact (compose inexact->exact round)))
  (check-true  (spl? inc))                                  ; an iso IS a spl
  (check-true  (iso? inc))
  (check-false (iso? int<-real))                            ; a mere spl is not an iso
  (check-equal? (check-spl-laws int<-real '(1 2 -7)) '())   ; the one-sided law holds
  (check-equal? (check-spl-laws inc '(0 5)) '())            ; the spl battery, free on isos
  ;; the OTHER side betrays the non-iso: to . from is normalize, not id
  (define e (spl-normalize int<-real))
  (check-equal? (e 1.5) 2.0)
  (check-equal? (e (e 1.5)) (e 1.5))                        ; idempotent
  (check-equal? (e 2.0) 2.0)                                ; identity on the section's image
  ;; compose: closed on spls; callable = to
  (check-equal? ((compose-spl int<-real (spl add1 sub1)) 3) 4.0)
  (check-equal? (int<-real 3) 3.0)

  ;; --- pure: the constant fn -- ignores its args, returns the v ... as values ---
  (check-equal? ((pure 5) 'a 'b) 5)                                            ; any args ignored
  (check-equal? (call-with-values (lambda () ((pure 1 2 3) 'x)) list) '(1 2 3)) ; variadic -> values
  (check-equal? (call-with-values (lambda () ((pure))) list) '())              ; no values

  ;; --- on: every argument projected through f, then op (any arity) ---
  (check-equal? ((on + abs) -3 4) 7)               ; abs each, then +
  (check-equal? ((on + abs) -1 2 -3) 6)            ; n-ary, not just binary
  (check-equal? ((on cons add1) 1 2) '(2 . 3))

  ;; --- pass: the thrush -- hold the args, feed ONE function, values-transparent ---
  (check-equal? ((pass 5) add1) 6)                 ; one function, one value
  (check-equal? (call-with-values
                 (lambda () ((pass 17 5) quotient/remainder)) list)
                '(3 2))                             ; a multi-value f passes through raw
  (check-equal? (call-with-values
                 (lambda () ((pass 3 4) (fork + * -))) list)
                '(7 12 -1))                         ; the old fan-out: fork, flipped

  ;; --- fork: the function-first twin -- each fn to the same arg(s), as values ---
  (check-equal? ((fork add1) 5) 6)                 ; one function, one arg
  (check-equal? (call-with-values
                 (lambda () ((fork + * -) 3 4)) list)
                '(7 12 -1))                         ; each f applied to (3 4)
  (check-equal? (call-with-values
                 (lambda () ((fork add1 values) 5)) list)
                '(6 5))                             ; fanout: read + pass-through (values = identity)

  ;; --- spread: spread-combine -- each function to its own argument, results combined
  ;;     by h.  (spread h f g) a b = (h (f a) (g b)).  Small arities inlined, 5+ tail. ---
  (check-equal? ((spread list add1 sub1) 10 20) '(11 19))               ; (list (add1 10) (sub1 20))
  (check-equal? ((spread + values string-length) 10 "abc") 13)          ; mixed per-arg preprocessors: (+ 10 3)
  (check-equal? ((spread list add1 sub1 -) 1 2 3) '(2 1 -3))            ; arity 3 (macro case)
  (check-equal? ((spread list add1 sub1 - add1) 1 2 3 4) '(2 1 -3 5))   ; arity 4 (macro case)
  (check-equal? ((spread list add1 sub1 - add1 sub1) 1 2 3 4 5) '(2 1 -3 5 4)) ; arity 5 (tail)
  ;; coerces each arg then folds, the make-summary shape (op preprocesses BOTH sides):
  (check-equal? ((variadic (spread + string-length string-length) 0) "ab" "cde") 5)   ; (+ 2 3)
  ;; STREAM-WISE: multi-value fns, h combining each position individually
  (define (two-of x) (values x (* 10 x)))
  (check-equal? (call-with-values (lambda () ((spread + two-of two-of) 1 2)) list)
                '(3 30))                                          ; columns: (+ 1 2), (+ 10 20)
  (check-equal? (call-with-values (lambda () ((spread list two-of two-of) 1 2)) list)
                '((1 2) (10 20)))                                 ; h = list: the TRANSPOSE
  (check-equal? (call-with-values (lambda () ((spread list two-of two-of two-of) 1 2 3)) list)
                '((1 2 3) (10 20 30)))                            ; n-ary tail, same law
  ;; --- parallel: the broadcast twin -- each fn gets the WHOLE tuple, h combines ---
  (check-equal? ((parallel list car cdr) '(1 . 2)) '(1 2))        ; (list (car p) (cdr p))
  (check-equal? ((parallel + car cdr) '(3 . 4)) 7)
  (check-equal? ((parallel list + *) 2 3) '(5 6))                 ; the whole tuple to both
  ;; stream-wise: same-world viewers ZIPPED into parallel lists, columnwise
  (define cuts (parallel list (lambda (p) (values (car p) (cdr p)))
                              (lambda (p) (values (cdr p) (car p)))))
  (check-equal? (call-with-values (lambda () (cuts '(1 . 2))) list)
                '((1 2) (2 1)))                                   ; Ls-and-Rs shape, no fork
  ;; = the compose/fork spelling it names
  (check-equal? (call-with-values (lambda () ((compose (spread list car cdr)
                                                       (fork values values)) '(1 . 2))) list)
                (call-with-values (lambda () ((parallel list car cdr) '(1 . 2))) list))

  ;; --- variadic: a monoid op + unit lifted to any arity; the fold seeds from the FIRST
  ;;     arg (id only for the empty call), so no (op id x) on the 2+-ary paths ---
  (check-equal? ((variadic + 0))         0)         ; nullary = the unit
  (check-equal? ((variadic + 0) 5)       5)         ; 1-ary keeps (op id a) = (+ 0 5)
  (check-equal? ((variadic + 0) 1 2)     3)         ; (+ 1 2) -- no leading (+ 0 ...)
  (check-equal? ((variadic + 0) 1 2 3 4) 10)        ; (+ (+ (+ 1 2) 3) 4)

  ;; --- scanl / scanr: the fold's intermediate values, seed at its own end ---
  (check-equal? (scanl + 0 '(1 2 3)) '(0 1 3 6))
  (check-equal? (scanr + 0 '(1 2 3)) '(6 5 3 0))
  (check-equal? (scanl + 0 '()) '(0))                              ; empty: just the seed
  (check-equal? (scanr + 0 '()) '(0))
  (check-equal? (scanl cons 'z '(a b)) '(z (z . a) ((z . a) . b))) ; arg order: (f acc x)
  (check-equal? (scanr cons 'z '(a b)) '((a b . z) (b . z) z))     ; arg order: (f x acc)
  ;; the last/first entry IS the full fold (non-commutative op pins the shape)
  (check-equal? (last  (scanl - 10 '(1 2 3))) (foldl (lambda (x a) (- a x)) 10 '(1 2 3)))
  (check-equal? (first (scanr - 10 '(1 2 3))) (foldr - 10 '(1 2 3)))

  ;; --- fixed: single value, multiple values, and a key projection ---
  (check-equal? ((fixed (lambda (n) (quotient n 2))) 100) 0)        ; halve to the fixpoint 0
  (check-equal? (call-with-values                                   ; multi-value: (a b) -> (b min)
                 (lambda () ((fixed (lambda (a b) (values b (min a b)))) 5 3)) list)
                '(3 3))
  ;; stop when a derived quantity settles -- here the tens digit -- via key:
  (check-equal? ((fixed sub1 = (lambda (n) (quotient n 10))) 25) 24)
  ;; a 3-value tuple exercises a macro-built clause; 5 values fall to the list tail:
  (check-equal? (call-with-values
                 (lambda () ((fixed (lambda (a b c) (values b c (min a b c)))) 9 5 7)) list)
                '(5 5 5))
  (check-equal? (call-with-values
                 (lambda () ((fixed (lambda (a b c d e) (values b c d e (min a b c d e)))) 5 4 3 2 1)) list)
                '(1 1 1 1 1))

  ;; --- WIP: lockstep -- equivalent twins agree, a bad twin and a shape split are caught ---
  (define sos (lockstep (lambda (xs) (apply + (map (lambda (x) (* x x)) xs)))
                        (lambda (xs) (foldl (lambda (x a) (+ a (* x x))) 0 xs))))
  (check-equal? (sos '(1 2 3 4)) 30)                          ; both branches run and agree
  (check-true (lockstep? sos))
  (check-exn #rx"fell out of step"                            ; a buggy refactor is caught
             (lambda () ((lockstep (lambda (n) (* n n)) (lambda (n) (* n 2))) 3)))
  ;; higher-order: the check defers down the currying to the comparable leaf
  (define adder (lockstep (lambda (a) (lambda (b) (+ a b)))
                          (lambda (a) (lambda (b) (- b (- a))))))
  (check-true (lockstep? (adder 10)))                         ; (adder 10) is itself a lockstep
  (check-equal? ((adder 10) 5) 15)
  (check-exn #rx"shape mismatch"                              ; fn vs value at the same stage
             (lambda () ((lockstep (lambda (a) (lambda (b) (+ a b)))
                                   (lambda (a) (+ a 100))) 2)))
  ;; multiple values: agreement checked per value-position
  (define mv (lockstep (lambda (a b) (values (+ a b) (* a b)))
                       (lambda (a b) (values (+ b a) (* b a)))))   ; commuted twin
  (check-equal? (call-with-values (lambda () (mv 3 4)) list) '(7 12))
  (check-exn #rx"fell out of step"                            ; one value column disagrees
             (lambda () ((lockstep (lambda (a b) (values a b))
                                   (lambda (a b) (values a (add1 b)))) 1 2)))
  (check-exn #rx"arity mismatch"                              ; differing tuple lengths
             (lambda () ((lockstep (lambda (x) (values x x))
                                   (lambda (x) x)) 5)))
  ;; on/off: born on; off runs only the trusted impl (the LAST by default), raw
  (define ordinary (lambda (xs) (apply + (map (lambda (x) (* x x)) xs))))
  (define tuned    (lambda (xs) (foldl (lambda (x a) (+ a (* x x))) 0 xs)))   ; trusted (last)
  (define ls (lockstep ordinary tuned))
  (check-eq? (lockstep-mode ls) 'on)                          ; born on
  (check-equal? (ls '(1 2 3)) 14)                             ; both run, agree
  (define fast (lockstep-off ls))
  (check-eq? (lockstep-mode fast) 'off)
  (check-equal? (fast '(1 2 3)) 14)                           ; runs tuned only, raw
  (check-eq? (lockstep-mode (lockstep-on fast)) 'on)          ; round-trips back
  ;; off short-circuits: a disagreeing impl is never run, so no error
  (check-equal? ((lockstep-off (lockstep (lambda (n) 'wrong) (lambda (n) (* n n)))) 3) 9)
  ;; #:trusted overrides which impl off runs
  (check-equal? ((lockstep-off (lockstep #:trusted 0 add1 sub1)) 10) 11)
  ;; on/off are no-ops on non-locksteps
  (check-eq? (lockstep-off ordinary) ordinary)
  (check-equal? (lockstep-on 42) 42)
  ;; off returns a raw function for HOFs -- no deferral, not a lockstep
  (define curr (lockstep (lambda (a) (lambda (b) (+ a b)))
                         (lambda (a) (lambda (b) (- b (- a))))))
  (check-true  (lockstep? (curr 10)))                         ; on: each stage is a lockstep
  (check-false (lockstep? ((lockstep-off curr) 10)))          ; off: plain closure
  (check-equal? (((lockstep-off curr) 10) 5) 15))
