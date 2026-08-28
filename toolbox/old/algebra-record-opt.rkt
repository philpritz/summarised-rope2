#lang racket

;; Small algebraic helpers -- isos, record optics over the values channel, and a few
;; combinators -- each documented at its definition (the canonical home other files
;; point to; the provide list is the surface map). Intended as a reusable helper
;; library, so some surface is built out past what this project strictly needs.
;; Narrative -- the iso group law, the optic protocol, the inlining rationale -- in
;; scribble/algebra.scrbl (pre-dates the lens->opt replacement; the van
;; Laarhoven generation is archived in deprecated/deprecated-7).

(provide (struct-out iso)        ; (iso to from); callable = applies `to`
         inverse-iso             ; the (from, to) swap -- (inverse-iso (iso f g)) = (iso g f)
         compose-iso             ; compose any number of isos; inverses reversed
         expt-iso                ; integer powers of an iso (scmutils function arithmetic)
         iso-law? check-iso-laws ; round-trip predicate; the inputs that fail it
         (struct-out opt)        ; (opt get set f); callable runs it -- the accessors ARE the ops
         opt-update              ; (opt-update o f*): the same opt, f* installed as its transform
         compose-opt             ; compose any number of opts, outer to inner; () = identity
         opt-from-peek           ; a store coalgebra (put-first peek) worn as an opt
         iso->opt                ; an iso worn as an opt (its put ignores the original -- that absence IS the iso)
         list-of                 ; map an element opt over a list -- ONE focus
                                 ;   (parallel lists: first focal, rest mapped context)
         lref                    ; index a list, fanning to N foci; length-safe
         ldiag                   ; the list diagonal -- view i, put broadcasts to all
         opt-lref opt-ldiag      ; row lifts over PARALLEL lists: a policy opt on the row,
                                 ;   signature-transparent (put returns P's output, listified)
         opt-list                ; the elementwise lift: P at EVERY index, all components listified
         focal                   ; the first-value-focal row policy (rest = read-only ctx)
         attach-viewer           ; join a viewer (a pure render over the view) onto an opt;
                                 ;   the render rides LAST, read-only -- the put is untouched
         varg                    ; rearrange the value stream by position
         vdiag                   ; the value-stream diagonal -- view i, put broadcasts to all (ldiag on values)
         pure                    ; (pure v ...): the constant fn, ignoring its args and returning the v ... as values (K)
         on                      ; (on op f) a ... = (op (f a) ...) -- Haskell's `on`
         arg                     ; project args by 0-based position
         pass                    ; apply each f to the fixed args, as values
         fork                    ; apply each f to the same arg(s), as values -- pass, functions-first
         spread                  ; apply each fn to its own arg, combine with h
         variadic                ; lift a binary op + seed to a variadic left fold
         fixed                   ; iterate to a fixed point
         scanl scanr             ; every intermediate fold value, seed included (length n+1)
         lexicographic)          ; first-difference 3-way order on sequences

;; A focused (to, from) pair; prop:procedure runs `to`, so an iso is callable as its
;; forward function -- only its own combinators see the other half.
(struct iso (to from)
  #:property prop:procedure (struct-field-index to))

(define (inverse-iso i) (iso (iso-from i) (iso-to i)))

;; Compose isos; the composite inverts the halves in reverse. (compose-iso) = identity.
(define (compose-iso . is)
  (iso (apply compose (map iso-to is))
       (apply compose (map iso-from (reverse is)))))

;; Integer powers of an iso, in the style of scmutils function arithmetic: n<0 uses
;; the inverse, so (expt-iso i -1) = (inverse-iso i). Closed on isos.
(define (expt-iso i n)
  (cond [(negative? n) (expt-iso (inverse-iso i) (- n))]
        [else (for/fold ([acc (iso values values)]) ([_ (in-range n)]) (compose-iso i acc))]))

(define (iso-law? i x) (equal? ((compose-iso (inverse-iso i) i) x) x))   ; x round-trips unchanged
(define (check-iso-laws i xs) (filter (lambda (x) (not (iso-law? i x))) xs))   ; '() = genuine iso

;; opt: an optic as a plain record -- three fields, one protocol over the values channel:
;;   get : ws ... -> (values focus ctx ...)    ; focus first, read-only context behind
;;   set : ((set new ...) ws ...) -> ws ...    ; curried, news first; never sees context
;;   f   : (focus ctx ...) -> new ...          ; the stored transform -- reads the whole
;;                                             ;   view, returns exactly what set consumes
;; Applying an opt runs it: set (f (get ws)) ws. With the get-put adapter as f (values
;; for a plain lens) that is the identity round-trip; opt-update installs a real edit.
;; The struct accessors ARE the view/set ops -- ((opt-get o) w), (((opt-set o) new) w) --
;; and Racket's `compose` threads multiple values, so context needs no extra plumbing.
(struct opt (get set f)
  #:property prop:procedure
  (lambda (o . ws)
    (apply (apply (compose (opt-set o) (opt-f o) (opt-get o)) ws) ws)))

(define (opt-update o f*) (struct-copy opt o [f f*]))          ; the same opt, f* installed

;; compose-opt: outer to inner, variadic; (compose-opt) = the identity opt (focus = the
;; world). The inner's world is the outer's WHOLE view -- context values ride through to
;; the leaf -- the setters chain, and the composite transform is the inner's (an outer
;; opt's own f is superseded under composition).
(define identity-opt (opt values (lambda news (lambda _ws (apply values news))) values))
(define (compose-opt2 b1 b2)
  (opt (compose (opt-get b2) (opt-get b1))
       (lambda news
         (lambda ws
           (apply (compose (lambda news1 (apply (apply (opt-set b1) news1) ws))
                           (apply (opt-set b2) news)
                           (opt-get b1))
                  ws)))
       (opt-f b2)))
(define (compose-opt . bs) (foldl (lambda (b acc) (compose-opt2 acc b)) identity-opt bs))

;; opt-from-peek: a store coalgebra `peek` worn as an opt (one or more foci inside a
;; structure that is itself one or more values).
;;   peek : structvals ... -> (values put focus ...)   ; put-back FIRST, then foci
;;   put  : newfocus ...   -> structvals ...
;; get sheds the put; set re-peeks and hands the news to the put; f = values, so a bare
;; run is the identity by the lens's own laws. Why put-first: scribble.
(define (opt-from-peek peek)
  (opt (compose (lambda (put . foci) (apply values foci)) peek)
       (lambda news
         (lambda ws (apply (compose (lambda (put . _) (apply put news)) peek) ws)))
       values))

;; iso->opt: an iso worn as an opt -- view is its forward map, put is its backward map. The put
;; ignores the original structure (only the new focus matters), which is exactly what makes it an
;; iso rather than a general lens; so (compose-opt some-opt (iso->opt i)) composes with no fuss.
(define (iso->opt i) (opt-from-peek (lambda (s) (values (iso-from i) (i s)))))

;; list-of: a single-focus element opt lifted over a list -- ONE focus (the list of
;; views); the put rebuilds element-wise. Over SEVERAL parallel lists the further
;; lists arrive mapped too, as read-only context behind; the put rebuilds the FIRST
;; list alone. Stays single-value until `lref` fans out.
(define (list-of el)
  (opt-from-peek (lambda (xs . more)
    (apply values
           (lambda (ys . _) (map (lambda (x y) (((opt-set el) y) x)) xs ys))
           (map (opt-get el) xs)
           (map (lambda (l) (map (opt-get el) l)) more)))))

;; lref: index a list at positions `is`, fanning into N foci; the put writes them back
;; into a copy. Length-safe -- overwrites slots, never reshapes.
(define (lref . is)
  (opt-from-peek (lambda (xs)
    (define v (list->vector xs))
    (apply values
           (lambda nf (define w (vector-copy v))
                      (for ([i (in-list is)] [x (in-list nf)]) (vector-set! w i x))
                      (vector->list w))
           (map (lambda (i) (vector-ref v i)) is)))))

;; ldiag: the list diagonal -- view position `i`, put broadcasts one value to every slot.
;; The collapsing twin of `(lref i)`; lawful only when the slots are already equal.
(define (ldiag i)
  (opt-from-peek (lambda (xs) (values (lambda (x) (make-list (length xs) x)) (list-ref xs i)))))

;; ---------- row lifts: parallel lists, policy-driven ----------
;; opt-lref: apply a ROW opt P at position n across PARALLEL lists -- the world is
;; P's world with every component wrapped in a list. Signature-transparent: the view
;; and the news are P's own, and the put returns exactly what P's put returns,
;; LISTIFIED -- P decides which coordinates change and how many lists come back
;; (a one-value policy composes under a focal-only outer set; a varg-style policy
;; returns every list rebuilt; a mismatch is an arity error at the seam, not a
;; silent drop).
(define (opt-lref n P)
  (define (row lists) (map (lambda (l) (list-ref l n)) lists))
  (opt (lambda lists (apply (opt-get P) (row lists)))
       (lambda news
         (lambda lists
           (define row* (call-with-values
                          (lambda () (apply (apply (opt-set P) news) (row lists))) list))
           (apply values (for/list ([l (in-list lists)] [x (in-list row*)])
                           (list-set l n x)))))
       (opt-f P)))

;; opt-ldiag: opt-lref's collapsing twin -- the same signature lift, but each
;; coordinate P's put returns is BROADCAST along its whole list rather than written
;; at n. Lawful only when each written list's slots are already equal.
(define (opt-ldiag n P)
  (define (row lists) (map (lambda (l) (list-ref l n)) lists))
  (opt (lambda lists (apply (opt-get P) (row lists)))
       (lambda news
         (lambda lists
           (define row* (call-with-values
                          (lambda () (apply (apply (opt-set P) news) (row lists))) list))
           (apply values (for/list ([l (in-list lists)] [x (in-list row*)])
                           (make-list (length l) x)))))
       (opt-f P)))

;; opt-list: lift an opt P to work ELEMENTWISE over parallel lists -- P applied at
;; every index, zipWith-style. The same signature law as opt-lref, at all indices
;; at once: P's world, view, news, and put output each with every component wrapped
;; in a list. (opt-list car-opt) recovers list-of; (opt-list P) over a k-component
;; world consumes k parallel lists. Assumes non-empty lists (an empty world has no
;; rows to reveal P's arities).
(define (opt-list P)
  (define (rows lists) (if (null? lists) '() (apply map list lists)))   ; transpose
  (define (per-row f) (lambda (r) (call-with-values (lambda () (apply f r)) list)))
  (opt (lambda lists
         (apply values (rows (map (per-row (opt-get P)) (rows lists)))))
       (lambda news
         (lambda lists
           (apply values
                  (rows (map (lambda (r ns) ((per-row (apply (opt-set P) ns)) r))
                             (rows lists) (rows news))))))
       (lambda viewlists
         (apply values (rows (map (per-row (opt-f P)) (rows viewlists)))))))

;; focal: the first-value-focal row policy -- view the whole values stream (first
;; writable, the rest read-only context); the put consumes ONE new first value and
;; returns it alone. (opt-lref n focal) is hence "edge n's focal slot, its context
;; in view" -- the common companion to the widened zipper optics.
(define focal (opt values (lambda (x) (lambda (v . _) x)) (lambda (v . _) v)))

;; attach-viewer: join a VIEWER -- a pure function over an opt's view values (a
;; render; no put half) -- onto an opt. The rendering rides LAST on the values
;; channel as read-only context; the put is o's own, and the default transform
;; sheds the rendering before delegating, so a bare run of the attached opt is a
;; bare run of o. An installed transform sees the full widened stream and returns
;; what o's set consumes. (opt-get o) is the opt->viewer coercion, `compose` the
;; viewer composition -- a viewer is a role, not a type.
(define (attach-viewer o viewer)
  (opt (lambda ws
         (define vs (call-with-values (lambda () (apply (opt-get o) ws)) list))
         (apply values (append vs (list (apply viewer vs)))))
       (opt-set o)
       (lambda all (apply (opt-f o) (drop-right all 1)))))

;; varg: the opt twin of `arg` -- focus the values at positions `is`, in order; the put
;; writes them back. Lawful for distinct positions; a repeated position is a lossy
;; diagonal (put-get fails).
(define (varg . is)
  (opt-from-peek (lambda structvals
    (define v (list->vector structvals))
    (apply values
           (lambda nf (define w (vector-copy v))
                      (for ([i (in-list is)] [x (in-list nf)]) (vector-set! w i x))
                      (apply values (vector->list w)))
           (map (lambda (i) (vector-ref v i)) is)))))

;; vdiag: the value-stream diagonal -- view value `i`, the put broadcasts one value to every
;; position. The value-stream twin of `ldiag` (and the collapsing twin of `varg`); lawful only
;; when the positions are already equal.
(define (vdiag i)
  (opt-from-peek (lambda structvals
    (values (lambda (x) (apply values (make-list (length structvals) x)))
            (list-ref structvals i)))))

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

;; pass: hold a tuple of args, then apply each function to them, as values --
;; ((pass . args) f g ...) = (values (apply f args) (apply g args) ...).
(define ((pass . args) . fs)
  (apply values (map (lambda (f) (apply f args)) fs)))

;; fork: the function-first twin of `pass` -- hold the functions, then apply each to the same
;; args, as values: ((fork f g ...) . args) = (values (apply f args) (apply g args) ...). A fanout;
;; ((fork f g) x) = (values (f x) (g x)), e.g. (fork read values) reads and passes its arg through.
(define ((fork . fs) . args)
  (apply values (map (lambda (f) (apply f args)) fs)))

;; spread: each function to its corresponding argument, results combined by `h` --
;; ((spread h f g ...) a b ...) = (h (f a) (g b) ...). case-lambda inlines 1..4 fns
;; positionally; 5+ falls to a map/apply tail. Used as (spread combine coerce coerce).
(define spread
  (case-lambda
    [(h f)       (lambda (a)       (h (f a)))]
    [(h f g)     (lambda (a b)     (h (f a) (g b)))]
    [(h f g k)   (lambda (a b c)   (h (f a) (g b) (k c)))]
    [(h f g k l) (lambda (a b c d) (h (f a) (g b) (k c) (l d)))]
    [(h . fs)    (lambda xs (apply h (map (lambda (f x) (f x)) fs xs)))]))

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
;; Self-contained -- delete this submodule to retract.
(module+ experimental
  (require (only-in racket/base [lambda %lambda]))    ; the genuine lambda, renamed
  (provide lambda)

  (define-syntax lambda
    (syntax-rules ()
      [(_ ((h . inner) . args) body ...)              ; parenthesised head -> peel one level
       (lambda (h . inner) (%lambda args body ...))]
      [(_ formals body ...)                           ; flat / rest / kw / optional -> real lambda
       (%lambda formals body ...)]))

  ;; staged-apply -- the curried binder's eliminator: one list per stage,
  ;;   (staged-apply f l1 l2 ...) = (apply (apply f l1) l2) ...
  (provide staged-apply)
  (define (staged-apply f . ls)
    (foldl (lambda (l g) (apply g l)) f ls))

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

  ;; --- pure: the constant fn -- ignores its args, returns the v ... as values ---
  (check-equal? ((pure 5) 'a 'b) 5)                                            ; any args ignored
  (check-equal? (call-with-values (lambda () ((pure 1 2 3) 'x)) list) '(1 2 3)) ; variadic -> values
  (check-equal? (call-with-values (lambda () ((pure))) list) '())              ; no values

  ;; --- on: every argument projected through f, then op (any arity) ---
  (check-equal? ((on + abs) -3 4) 7)               ; abs each, then +
  (check-equal? ((on + abs) -1 2 -3) 6)            ; n-ary, not just binary
  (check-equal? ((on cons add1) 1 2) '(2 . 3))

  ;; --- pass: hold the args, apply several functions to them, as values ---
  (check-equal? ((pass 5) add1) 6)                 ; one function, one value
  (check-equal? (call-with-values
                 (lambda () ((pass 3 4) + * -)) list)
                '(7 12 -1))                         ; each f applied to (3 4), as values

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

  ;; --- opt: a peek worn as an opt (put FIRST), the accessors as ops, composition, the laws ---
  (define fst-opt                           ; an opt onto a list's head (one focus)
    (opt-from-peek (lambda (xs) (values (lambda (x) (cons x (cdr xs))) (first xs)))))
  (check-equal? ((opt-get fst-opt) '(1 2 3)) 1)
  (check-equal? ((compose add1 (opt-get fst-opt)) '(1 2 3)) 2)                     ; fold the view: compose threads it
  (check-equal? (((opt-set fst-opt) 9) '(1 2 3)) '(9 2 3))
  (check-equal? ((opt-update fst-opt add1) '(1 2 3)) '(2 2 3))
  (check-equal? (fst-opt '(1 2 3)) '(1 2 3))                                       ; bare run (f = values) = identity
  (check-equal? (((opt-set fst-opt) ((opt-get fst-opt) '(1 2))) '(1 2)) '(1 2))    ; get-put
  (check-equal? ((opt-get fst-opt) (((opt-set fst-opt) 9) '(1 2))) 9)              ; put-get
  (check-equal? (((opt-set fst-opt) 8) (((opt-set fst-opt) 9) '(1 2)))              ; put-put
                (((opt-set fst-opt) 8) '(1 2)))
  ;; composition chains the setters through the outer's view: onto first-of-first
  (define fst-fst (compose-opt fst-opt fst-opt))
  (check-equal? ((opt-get fst-fst) '((1 2) 3)) 1)
  (check-equal? (((opt-set fst-fst) 9) '((1 2) 3)) '((9 2) 3))
  (check-equal? ((opt-get (compose-opt)) 42) 42)                                   ; empty = the identity opt
  (check-equal? (((opt-set (compose-opt)) 9) 42) 9)

  ;; --- list-of (one list focus), lref (fan-out to N foci), varg (rearrange by position) ---
  (define (vlist l . s) (call-with-values (lambda () (apply (opt-get l) s)) list)) ; collect the view's values
  (define li (list-of (opt-from-peek (lambda (p) (values (lambda (x) (cons x (cdr p))) (car p))))))  ; car-opt over a list
  (define gl (list (cons 1 'g) (cons 2 'g) (cons 3 'g)))
  (check-equal? (vlist li gl) (list '(1 2 3)))                      ; list-of is ONE focus: the list
  (check-equal? (((opt-set li) (list 10 20 30)) gl)
                (list (cons 10 'g) (cons 20 'g) (cons 30 'g)))
  (define L (compose-opt li (lref 0 2)))
  (check-equal? (vlist L gl) '(1 3))                               ; lref fans to N foci
  (check-equal? (((opt-set L) 'X 'Y) gl) (list (cons 'X 'g) (cons 2 'g) (cons 'Y 'g)))
  (check-equal? ((opt-update L (lambda (a b) (values (add1 a) (add1 b)))) gl)      ; one f over the whole view
                (list (cons 2 'g) (cons 2 'g) (cons 4 'g)))
  (check-equal? (length (((opt-set L) 'X) gl)) 3)                  ; under-supplied: length preserved
  (check-equal? (vlist L (((opt-set L) 'X 'Y) gl)) '(X Y))         ; put-get
  (check-equal? (vlist (compose-opt li (lref 0 1 2) (varg 2 0)) gl) '(3 1))        ; varg reorders
  (check-equal? (vlist (compose-opt li (lref 0 1 2) (varg 2 0)) gl)
                (call-with-values (lambda () ((arg 2 0) 1 2 3)) list))            ; the view of varg = arg

  ;; ldiag: view position i (the bias), the put broadcasts one value to every slot
  (check-equal? ((opt-get (ldiag 0)) '(a b c)) 'a)
  (check-equal? ((opt-get (ldiag 1)) '(a b c)) 'b)
  (check-equal? (((opt-set (ldiag 0)) 'X) '(a b c)) '(X X X))
  (check-equal? ((opt-update (ldiag 1) symbol->string) '(a b c)) '("b" "b" "b"))

  ;; --- row lifts over parallel lists: the policy opt decides get AND set ---
  ;; focal policy: whole row in view, put writes the first list's slot, returns it alone
  (check-equal? (vlist (opt-lref 1 focal) '(a b c) '(x y z) '(1 2 3)) '(b y 2))
  (check-equal? (((opt-set (opt-lref 1 focal)) 'B) '(a b c) '(x y z)) '(a B c))
  (check-equal? ((opt-update (opt-lref 1 focal) (lambda (b y) y))           ; read ctx, write focal
                 '(a b c) '(x y z))
                '(a y c))
  ;; varg policy: symmetric world -- the put returns EVERY list rebuilt (P's arity, listified)
  (check-equal? (call-with-values
                  (lambda () (((opt-set (opt-lref 1 (varg 0))) 'B) '(a b c) '(x y z))) list)
                '((a B c) (x y z)))
  ;; vdiag policy: broadcast ACROSS the lists at index n (both lists' slot 1 become 'Q)
  (check-equal? (call-with-values
                  (lambda () (((opt-set (opt-lref 1 (vdiag 0))) 'Q) '(a b c) '(x y z))) list)
                '((a Q c) (x Q z)))
  ;; opt-ldiag: the same lift, broadcasting ALONG each written list
  (check-equal? (vlist (opt-ldiag 1 focal) '(a b c) '(1 2 3)) '(b 2))
  (check-equal? (((opt-set (opt-ldiag 0 focal)) 'X) '(a b c) '(1 2 3)) '(X X X))
  ;; single list, identity-ish policy: the old shapes recovered as instances
  (check-equal? (vlist (opt-lref 1 focal) '(a b c)) '(b))
  (check-equal? (((opt-set (opt-ldiag 1 focal)) 'X) '(a b c)) '(X X X))
  ;; list-of over parallel lists: each further list arrives mapped through the element opt
  (define el (opt-from-peek (lambda (p) (values (lambda (x) (cons x (cdr p))) (car p)))))
  (check-equal? (vlist (list-of el) (list '(1 . g) '(2 . g)) (list '(3 . h) '(4 . h)))
                '((1 2) (3 4)))
  (check-equal? (((opt-set (list-of el)) '(10 20)) (list '(1 . g) '(2 . g)) (list '(3 . h) '(4 . h)))
                (list '(10 . g) '(20 . g)))

  ;; --- opt-list: the elementwise lift -- P at every index, everything listified ---
  ;; a one-component P recovers list-of's shape
  (check-equal? (vlist (opt-list el) (list '(1 . g) '(2 . g))) '((1 2)))
  (check-equal? (((opt-set (opt-list el)) '(10 20)) (list '(1 . g) '(2 . g)))
                (list '(10 . g) '(20 . g)))
  ;; a k-component P consumes k parallel lists; view/news/output all listified
  (check-equal? (vlist (opt-list focal) '(a b) '(1 2)) '((a b) (1 2)))
  (check-equal? (((opt-set (opt-list focal)) '(X Y)) '(a b) '(1 2)) '(X Y))
  (check-equal? ((opt-update (opt-list focal) (lambda (vs cs) cs)) '(a b) '(1 2))
                '(1 2))                                           ; per-element: write ctx into focal
  ;; bare run = P's bare run, elementwise (the lifted default transform)
  (check-equal? ((opt-list focal) '(a b) '(1 2)) '(a b))

  ;; --- attach-viewer: the render rides last, the put untouched ---
  (define shown (attach-viewer fst-opt ~a))                     ; render the head as a string
  (check-equal? (vlist shown '(1 2)) '(1 "1"))                  ; view widened by the render
  (check-equal? (((opt-set shown) 9) '(1 2)) '(9 2))            ; put = fst-opt's own
  (check-equal? (shown '(1 2)) '(1 2))                          ; bare run = o's bare run
  (check-equal? ((opt-update shown (lambda (v r) (string-length r))) '(10 2))
                '(2 2))                                          ; a transform sees the render
  ;; a multi-value view: the render folds the whole stream, set stays the policy's
  (define row+ (attach-viewer (opt-lref 1 focal) list))
  (check-equal? (vlist row+ '(a b c) '(x y z)) '(b y (b y)))
  (check-equal? (((opt-set row+) 'B) '(a b c) '(x y z)) '(a B c))

  ;; vdiag: ldiag on the value stream -- view value i, the put broadcasts to every position
  (check-equal? ((opt-get (vdiag 0)) 'a 'b 'c) 'a)
  (check-equal? ((opt-get (vdiag 1)) 'a 'b 'c) 'b)
  (check-equal? (call-with-values (lambda () (((opt-set (vdiag 0)) 'X) 'a 'b 'c)) list) '(X X X))
  (check-equal? (call-with-values (lambda () ((opt-update (vdiag 1) symbol->string) 'a 'b 'c)) list)
                '("b" "b" "b"))

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
