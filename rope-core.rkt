#lang racket

(require racket/generic
         (only-in "toolbox/main.rkt" on variadic spread))

;; Summarised rope: a persistent rope caching a user-defined summary at every node.
;; Three factories make the surface:
;;   smr        : (string | rope | summary)* -> summary    (make-summary)
;;   make-rope  : smr -> ((string | rope)* -> rope)         (rebalancing factory)
;;   multisect  : smr guide... -> (rope -> piece values)    (the one split primitive)
;; Two halves behind an abstraction barrier: PART 1 fusing/non-balancing rope, PART 2 guided/balancing descent.
;; Narrative in scribble/rope-core.scrbl.

(define smr/c   procedure?)
(define guide/c (-> any/c any/c (or/c -1 0 1)))

(provide
 rope? rope-algebra                          ; the algebra a rope was BUILT with -- the
                                             ;   wear-transparent derivation channel (a
                                             ;   worn algebra, e.g. a memoized bundle,
                                             ;   derives as itself; an owner thunk can't)
 rope-length rope-hash                       ; NEW in this repo (not in the old repo's
                                             ;   copy): INTRINSIC per-node metadata --
                                             ;   char count and Karp-Rabin content
                                             ;   fingerprint, carried whatever the user
                                             ;   algebra, so keys need no bundle. See kr.
 gen:summary-part part->summary summary-part?
 (contract-out
  [make-summary (-> (-> string? any/c) (-> any/c any/c any/c) smr/c)]
  [make-rope    (-> smr/c (->* () #:rest (listof (or/c string? rope?)) rope?))]
  [multisect    (->* (smr/c) () #:rest (listof guide/c) (-> rope? any))]
  [frame        (-> smr/c any/c any/c (-> guide/c guide/c))]))

(module+ internal
  (provide rope-leaves rope-height rope-summary
           leaf? branch? leaf-text
           branch-left branch-right))

;; ---------- PART 1: the fusing, non-balancing rope ----------

;; NEW in this repo: the intrinsic content fingerprint -- a Karp-Rabin rolling
;; hash as a monoid (after summaries' hash-smr, which rope-core cannot require),
;; carried by EVERY node beside leaves/height, independent of the user algebra.
;; value = (kr h scale), scale = B^len mod M; the join is O(1), so a branch's
;; fp costs one multiply-add over its children's. Shape-blind: equal text =>
;; equal fp whatever the tree shape, which is what makes it a content KEY that
;; survives rebalancing. Probabilistic identity (M ~ 2^61).
(struct kr (h scale) #:transparent)
(define kr-M (- (expt 2 61) 1))                  ; Mersenne prime
(define kr-B 1000003)
(define (kr-leaf s)
  (for/fold ([h 0] [sc 1] #:result (kr h sc)) ([c (in-string s)])
    (values (modulo (+ (* h kr-B) (char->integer c)) kr-M)
            (modulo (* sc kr-B) kr-M))))
(define (kr-join x y)
  (kr (modulo (+ (* (kr-h x) (kr-scale y)) (kr-h y)) kr-M)
      (modulo (* (kr-scale x) (kr-scale y)) kr-M)))

(struct rope (summary algebra leaves height length hash) #:transparent
  #:property prop:custom-write (lambda (r port mode) (rope-write-text r port)))
(struct leaf   rope (text)       #:transparent)
(struct branch rope (left right) #:transparent
  #:guard (lambda (summary algebra leaves height length hash left right _name)
            (when (or (zero? (rope-leaves left)) (zero? (rope-leaves right)))
              (error 'branch "empty child -- branches hold two non-empty ropes"))
            (values summary algebra leaves height length hash left right)))

(define max-leaf 32)

(define-generics summary-part
  (part->summary summary-part smr [fail]))   ; fail: hash-ref's convention -- a value,
                                             ;   or a thunk to call; absent = error

(define (make-summary string-summary combine)
  (define id (string-summary ""))
  (define (coerce x)
    (match x
      [""                                        id]
      [(? string?)                               (string-summary x)]
      [(? rope?)                                 (smr (rope-summary x))]
      [(? summary-part?)                         (part->summary x smr)]
      [_                                         x]))
  ;; smr folds `combine` over coerced args via `variadic`, which seeds the fold from the
  ;; FIRST argument, not from id -- valid by the identity law (combine id x) = x. `op`
  ;; coerces BOTH sides (was `values coerce`): the id-seed used to be what coerced the first
  ;; argument, so with the seed gone the op must. See variadic in toolbox/algebra.rkt.
  (define smr (variadic (spread combine coerce coerce) id))
  smr)

(define ((leaf-rope smr) text)
  (if (string=? text "")
      (leaf (smr "") smr 0 0 0 (kr 0 1) "")
      (leaf (smr text) smr 1 0 (string-length text) (kr-leaf text) text)))
(define ((branch-rope smr) l r)
  (branch (smr l r) smr
          (+ (rope-leaves l) (rope-leaves r))
          (add1 (max (rope-height l) (rope-height r)))
          (+ (rope-length l) (rope-length r))
          (kr-join (rope-hash l) (rope-hash r))
          l r))

;; rope-split's inverse: (rope-join (rope-split t)) = t. fuses any fusable seam it builds,
;; keeping the no-fusable-adjacent-pair invariant -- so one join serves both balance and guided cuts.
(define (rope-join l r)
  (define smr (rope-algebra l))
  (define (chars lf) (string-length (leaf-text lf)))
  (let join ([l l] [r r])
    (match* (l r)
      [(_ _) #:when (zero? (rope-leaves l)) r]            ; empties drop
      [(_ _) #:when (zero? (rope-leaves r)) l]
      [((? leaf?) (? leaf?))                              ; two leaves at the seam:
       (if (<= (+ (chars l) (chars r)) max-leaf)
           ((leaf-rope smr) (string-append (leaf-text l) (leaf-text r)))   ; fuse if they fit,
           ((branch-rope smr) l r))]                                       ; else branch
      [((branch _ _ _ _ _ _ ll lr) _) #:when (and (leaf? lr) (< (chars lr) max-leaf))
       ((branch-rope smr) ll (join lr r))]                                 ; small right leaf tip of l
      [(_ (branch _ _ _ _ _ _ rl rr)) #:when (and (leaf? rl) (< (chars rl) max-leaf))
       ((branch-rope smr) (join l rl) rr)]                                 ; small left leaf tip of r
      [(_ _) ((branch-rope smr) l r)])))

(define (rope-write-text r port)
  (cond
    [(leaf? r)   (write-string (leaf-text r) port)]
    [(branch? r) (rope-write-text (branch-left r) port)
                 (rope-write-text (branch-right r) port)]))

;; the boundary
(define (rope-split t)
  (match t
    [(branch _ _ _ _ _ _ l r) (values l r)]
    [(? leaf?)
     (define s (rope-algebra t)) (define str (leaf-text t))
     (define mid (quotient (string-length str) 2))
     (values ((leaf-rope s) (substring str 0 mid))
             ((leaf-rope s) (substring str mid)))]))

(define (rope-info t)
  (list (rope-summary t) (rope-leaves t) (rope-height t)))

(define (rope-zero t)
  (list ((rope-algebra t) "") 0 0))

(define (empty-rope? t) (zero? (rope-leaves t)))   ; no leaves -> the empty rope

(define ((combine-info smr) a b)
  (list (smr (first a) (first b))
        (+   (second a) (second b))
        (max (third a)  (third b))))

;; ---------- PART 2: guided & balanced descent ----------

(define ((within-ratio a) l r)
  (cond [(<= (max l r) (+ (* a (min l r)) 1)) 0]
        [(> r l)  1]
        [else    -1]))

;; the one descent; used internally only, not exported.
(define ((bisect cmb) t [decide (on (within-ratio 3) second)])
  (define mt ((leaf-rope (rope-algebra t)) ""))   ; the empty rope, minted once
  (let descend ([before (rope-zero t)] [t t] [after (rope-zero t)])
    (define-values (l r) (rope-split t))
    (define il (rope-info l)) (define ir (rope-info r))
    (define L (cmb before il))
    (define R (cmb ir after))
    (cond
      [(or (empty-rope? l) (empty-rope? r))      ; a half empty -> t is a lone char, indivisible
       (if (positive? (decide L R)) (values t mt) (values mt t))]   ; the whole piece, cut after / before
      [else
       (match (decide L R)
         [ 0 (values l r)]                                                                ; cut is here
         [ 1 (let-values ([(rl rr) (descend L r after)]) (values (rope-join l rl) rr))]   ; cut lies right
         [-1 (let-values ([(ll lr) (descend before l R)]) (values ll (rope-join lr r)))])])))  ; cut lies left

;; bake outer context (b, a) into a guide so it judges as if it saw the whole document. Used
;; transiently to judge a cut, then discarded -- never frame-and-store (see scribble Internals).
(define ((frame combine b a) g)
  (lambda (l r) (g (combine b l) (combine r a))))

;; the rope's general-purpose splitting operation: n guides -> n+1 pieces.
;; the guides come as rest args -- (multisect smr) bisects, (multisect smr gs ge) carves a cursor.
(define ((multisect smr . guides) t)
  (define cmb (combine-info smr))
  (if (null? guides)
      ((bisect cmb) t)
      (for/fold ([rest t] [bacc (rope-zero t)] [pieces '()]
                 #:result (apply values (reverse (cons rest pieces))))
                ([g (in-list guides)])
        (let-values ([(l r) ((bisect cmb) rest ((frame cmb bacc (rope-zero t)) (on g first)))])
          (values r (cmb bacc (rope-info l)) (cons l pieces))))))

(define (make-rope smr)
  (define mt    ((leaf-rope smr) ""))
  (define cmb   (combine-info smr))
  (define halve (bisect cmb))
  (define (chunk s)
    (for/list ([start (in-range 0 (string-length s) max-leaf)])
      (substring s start (min (string-length s) (+ start max-leaf)))))
  (define (coerce x)
    (match x
      [""          mt]
      [(? string?) (foldr rope-join mt (map (leaf-rope smr) (chunk x)))]
      [_           x]))
  (define (pathological? t)
    (match-define (list _ leaves height) (rope-info t))
    (> height (+ (* 3 (log (add1 leaves) 2)) 2)))
  (define (rebalance t)
    (if (zero? (third (rope-info t)))
        t
        (let-values ([(l r) (halve t (on (within-ratio 2) second))])
          (rope-join (rebalance l) (rebalance r)))))
  ;; coerce each part, then fold with rope-join.
  (define (build . parts)
    (define t (foldr rope-join mt (map coerce parts)))
    (if (pathological? t) (rebalance t) t))
  build)

;; ========== EXPERIMENTAL: variable multisect (guide*) ==========================
;; Provisional, opt-in: (require (submod "rope-core.rkt" experimental)). Where a guide
;; names one cut, a guide* reports cuts across a region, so the piece count varies with
;; the text (e.g. split at every newline). Self-contained -- delete this submodule to
;; retract. It sees PART 1/2's privates (rope-split, rope-join, rope-summary) directly.
(module+ experimental
  (provide (struct-out guide*) make-guide* frame-guide* multisect*)

  ;; queried with the two child summaries, answers (values left? mid? right?): a cut inside
  ;; the left child, at the seam, inside the right child. bs/as are the outer context,
  ;; stored by frame-guide* and re-folded as the walk descends.
  (struct guide* (cmb bs as raw)
    #:property prop:procedure
    (lambda (d fsl fsr) ((guide*-raw d) (guide*-bs d) fsl fsr (guide*-as d))))

  (define (make-guide* cmb raw) (guide* cmb (cmb) (cmb) raw))   ; (cmb) = the algebra's identity

  (define (frame-guide* g bl ar)                                ; fold new context onto both sides
    (struct-copy guide* g
      [bs ((guide*-cmb g) (guide*-bs g) bl)]
      [as ((guide*-cmb g) ar (guide*-as g))]))

  (define (point? t) (and (leaf? t) (< (string-length (leaf-text t)) 2)))  ; no boundary inside

  ;; walk t, harvesting every cut the guide* finds; prune subtrees with none. Tail-passing:
  ;; push conses onto the running piece list, welding across a seam the guide* left uncut.
  (define ((multisect* g0) t)
    (define cmb (guide*-cmb g0))
    (define (push t fuse? tail)
      (if (and fuse? (pair? tail))
          (cons (rope-join t (car tail)) (cdr tail))
          (cons t tail)))
    (let go ([t t] [g g0] [fuse? #f] [tail '()])
      (if (point? t)
          (push t fuse? tail)
          (let*-values ([(l r)         (rope-split t)]
                        [(fsl fsr)     (values (rope-summary l) (rope-summary r))]
                        [(lft mid rgt) (g fsl fsr)])
            (let ([tail (if rgt (go r (frame-guide* g fsl (cmb)) fuse? tail)
                                (push r fuse? tail))])
              (if lft (go l (frame-guide* g (cmb) fsr) (not mid) tail)
                      (push l (not mid) tail)))))))

  (module+ test
    (require rackunit)
    (define sum   (make-summary string-length +))
    (define build (make-rope sum))
    (define (texts ps) (map (lambda (p) (format "~a" p)) ps))
    ;; mid? always fires -> a cut between every char; left?/right? = an interior boundary exists
    (define cut-each (make-guide* sum (lambda (bs l m r) (values (> l 1) (> l 0) (> r 1)))))
    (check-equal? (texts ((multisect* cut-each) (build "abcd"))) '("a" "b" "c" "d"))
    ;; nothing fires -> the whole rope welds back to one piece
    (define no-cut (make-guide* sum (lambda (bs l m r) (values #f #f #f))))
    (check-equal? (texts ((multisect* no-cut) (build "abcd"))) '("abcd"))))

(module+ test
  (require rackunit)

  (define sum (make-summary string-length +))

  (define (spine n)
    (let loop ([i n])
      (if (= i 1)
          ((leaf-rope sum) (make-string max-leaf #\x))
          ((branch-rope sum) ((leaf-rope sum) (make-string max-leaf #\x)) (loop (sub1 i))))))

  (define (halve t) ((bisect (combine-info sum)) t))

  (define r ((make-rope sum) "abcdef"))
  (check-equal? (~a r) "abcdef")
  (check-equal? (sum r) 6)

  (define r2 ((make-rope sum) "the quick brown fox jumps over the lazy dog"))
  (check-false  (leaf? r2))
  (check-equal? (~a r2) "the quick brown fox jumps over the lazy dog")
  (check-equal? (sum r2) 43)

  (check-equal? (sum "ab" r "x") (+ 2 6 1))
  (check-equal? (sum 5 r)        (+ 5 6))
  (check-equal? (sum "")         0)
  (check-equal? (sum)            0)

  (define joined ((make-rope sum) "(" r ")"))
  (check-equal? (~a joined) "(abcdef)")
  (check-equal? (sum joined) 8)

  (define sum2 (make-summary string-length +))
  (check-equal? (sum2 r) 6)

  (define-values (l rr) (halve r))
  (check-equal? (string-append (~a l) (~a rr)) "abcdef")

  (let-values ([(a b) (halve ((make-rope sum)))])
    (check-true (equal? a ((make-rope sum))))
    (check-true (equal? b ((make-rope sum)))))

  (let-values ([(lh rh) (halve ((make-rope sum) "x"))])
    (check-equal? (~a (rope-join lh rh)) "x"))
  (let ([e ((make-rope sum))] [ab ((make-rope sum) "ab")])
    (check-true   (equal? (rope-join e e) e))
    (check-equal? (~a (rope-join e (rope-join ab e))) "ab"))

  (let-values ([(sl sr) (halve (spine 8))])
    (check-equal? (string-append (~a sl) (~a sr)) (make-string (* 8 max-leaf) #\x))
    (check-true (<= (max (rope-leaves sl) (rope-leaves sr))
                    (+ (* 3 (min (rope-leaves sl) (rope-leaves sr))) 1))))

  (let* ([phrase ((make-rope sum) "the quick brown fox jumps over the lazy dog")]
         [at17 (lambda (L R) (cond [(< L 17) 1] [(> L 17) -1] [else 0]))])
    (let-values ([(lft rgt) ((multisect sum at17) phrase)])
      (check-equal? (~a lft) "the quick brown f")
      (check-equal? (~a rgt) "ox jumps over the lazy dog")
      (check-equal? (string-append (~a lft) (~a rgt))
                    "the quick brown fox jumps over the lazy dog")))

  (define big ((make-rope sum) (make-string 2048 #\x)))
  (check-equal? (~a big) (make-string 2048 #\x))
  (check-true (<= (rope-height big)
                  (+ (* 3 (log (add1 (rope-leaves big)) 2)) 2)))

  (let ([j (rope-join ((make-rope sum) "ab") ((make-rope sum) "cd"))])
    (check-true   (leaf? j))
    (check-equal? (~a j) "abcd"))

  (let* ([lf   (leaf-rope sum)]
         [br   (branch-rope sum)]
         [mid  ((make-rope sum) (make-string 3000 #\z))]
         [frag (br (lf "c") (br mid (lf "d")))]
         [whole (rope-join (lf "ab") frag)])
    (define (leftmost t) (if (leaf? t) t (leftmost (branch-left t))))
    (check-equal? (~a whole) (string-append "abc" (make-string 3000 #\z) "d"))
    (check-equal? (leaf-text (leftmost whole)) "abc"))

  (let ([t ((make-rope sum) (make-string 3000 #\a))])
    (define (leaf-count t) (if (leaf? t) 1 (+ (leaf-count (branch-left t)) (leaf-count (branch-right t)))))
    (check-equal? (~a t) (make-string 3000 #\a))
    (check-equal? (leaf-count t) (ceiling (/ 3000 max-leaf)))
    (check-equal? (rope-leaves t) (leaf-count t)))

  (check-equal? (rope-leaves r)  1)
  (check-equal? (rope-leaves r2) 2)

  ;; --- rope-hash / rope-length: intrinsic content identity, shape-blind ---
  (check-equal? (rope-hash ((branch-rope sum) ((leaf-rope sum) "ab") ((leaf-rope sum) "cd")))
                (rope-hash ((leaf-rope sum) "abcd")))          ; shape-blind
  (check-false  (equal? (rope-hash ((make-rope sum) "ab"))
                        (rope-hash ((make-rope sum) "ba"))))   ; order distinguishes
  (check-equal? (rope-hash ((make-rope sum)))
                (rope-hash ((make-rope sum) "")))              ; the identity
  (check-equal? (rope-length ((make-rope sum) "ab" "cde")) 5)
  (check-equal? (rope-length ((make-rope sum))) 0)
  (let ([big ((make-rope sum) (make-string 3000 #\q))])        ; survives chunking + rebalance
    (check-equal? (rope-hash big) (rope-hash ((leaf-rope sum) (make-string 3000 #\q))))
    (check-equal? (rope-length big) 3000)))
