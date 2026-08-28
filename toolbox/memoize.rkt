#lang racket

;; Memoize: wear a PURE function with an exact-LRU cache. The wearing is a
;; CHAPERONE of f -- (memoize f) returns f's own identity with its application
;; replaced by the cache, so everything else about f survives: an applicable
;; STRUCT keeps its predicate and accessors (a memoized iso is still iso?, its
;; iso-from still there), a plain procedure keeps its arity. unsafe-chaperone-
;; procedure is what allows the cache to SKIP the underlying call (a safe
;; chaperone always invokes it); the "unsafe" is exactly our precondition --
;; nothing checks the replacement is equivalent, and for a pure f it is.
;;   (memoize f #:key k #:cap n)
;;   #:key  args -> the cache key (default: the arg list, equal?-keyed). The key
;;          is the call's SEMANTIC identity -- a derived key, cheaper or coarser
;;          than the args, is the main tuning point.
;;   #:cap  live entries; past it the least-recently-USED entry is evicted, so
;;          the working set survives any cache pressure (no wholesale flush).
;; Results are captured as value TUPLES, so multiple-value functions memoize
;; fine. Sound only for pure functions -- same caveat as lockstep's.
;; The cache itself is lru.rkt's LRU map (the engine used to live here; it was
;; split out when trie.rkt became its second consumer).

(require racket/unsafe/ops "lru.rkt")

(provide memoize                 ; f #:key #:cap -> f itself, worn cached (a chaperone)
         memo?                   ; recognizes a memoized value
         memo-size               ; live entries
         memo-clear!             ; drop every entry
         memo-on! memo-off!)     ; NEW in this repo (not in the old repo's copy):
                                 ;   the switch. Off = every call passes straight
                                 ;   through to f -- no lookup, no fill, no
                                 ;   eviction pressure; entries KEEP, so switching
                                 ;   back on resumes with the cache warm.
                                 ; ALSO NEW: #f from #:key is a reserved answer --
                                 ;   "don't cache this call": it passes through
                                 ;   like the switch, per call. A key fn whose
                                 ;   honest key could be #f must wrap it.

;; State rides in a WEAK side registry keyed by the chaperone (the memoized
;; value carries no fields of its own -- it IS f, to every observer but the
;; cache): the lru maps key -> result tuple; hit/miss is one lru-ref!.
(struct state (key lru [on? #:mutable]))
(define registry (make-weak-hasheq))          ; memoized value -> its state

(define (memoize f #:key [key list] #:cap [cap 4096])
  (define st (state key (make-lru cap) #t))
  (define (run . args)
    (define k (and (state-on? st) (apply (state-key st) args)))
    (if k
        (apply values
               (lru-ref! (state-lru st) k
                         (lambda () (call-with-values (lambda () (apply f args)) list))))
        (apply f args)))
  (define m (unsafe-chaperone-procedure f run))
  (hash-set! registry m st)
  m)

(define (memo-state who m)
  (or (hash-ref registry m #f) (raise-argument-error who "memo?" m)))
(define (memo? v) (and (hash-ref registry v #f) #t))
(define (memo-size m) (lru-count (state-lru (memo-state 'memo-size m))))
(define (memo-clear! m) (lru-clear! (state-lru (memo-state 'memo-clear! m))))
(define (memo-on!  m) (set-state-on?! (memo-state 'memo-on!  m) #t))
(define (memo-off! m) (set-state-on?! (memo-state 'memo-off! m) #f))

;; ============================================================================
(module+ test
  (require rackunit)

  ;; --- memoize: hits, multiple values, key projection ---
  (define calls (box 0))
  (define qr (memoize (lambda (x y) (set-box! calls (add1 (unbox calls)))
                                    (quotient/remainder x y))))
  (check-equal? (call-with-values (lambda () (qr 17 5)) list) '(3 2))
  (check-equal? (call-with-values (lambda () (qr 17 5)) list) '(3 2))
  (check-equal? (unbox calls) 1)                        ; the second call hit
  (check-true (memo? qr))
  (check-false (memo? add1))
  (define keyed (memoize string-length #:key string-upcase))
  (check-equal? (keyed "ab") 2)
  (check-equal? (keyed "AB") 2)                         ; same key -> hit

  ;; --- the chaperone wearing: everything about f survives ---
  (struct pair2 (to from) #:property prop:procedure (struct-field-index to))
  (define lexed (box 0))
  (define p (pair2 (lambda (x) (set-box! lexed (add1 (unbox lexed))) (* x x)) sub1))
  (define mp (memoize p))
  (check-true  (pair2? mp))                             ; the struct TYPE survives
  (check-equal? ((pair2-from mp) 10) 9)                 ; the other field still reachable
  (check-true  (chaperone-of? mp p))
  (check-equal? (mp 3) 9)
  (check-equal? (mp 3) 9)
  (check-equal? (unbox lexed) 1)                        ; ...and application is cached
  ;; a plain procedure keeps its arity
  (check-equal? (procedure-arity (memoize add1)) 1)

  ;; --- LRU: the working set survives; the stale tail is what dies ---
  (define seen '())
  (define lru (memoize (lambda (x) (set! seen (cons x seen)) x) #:cap 3))
  (for ([x '(a b c)]) (lru x))                          ; cache: (c b a)
  (void (lru 'a))                                       ; touch a: (a c b)
  (void (lru 'd))                                       ; over cap: evicts b, NOT a
  (check-equal? (memo-size lru) 3)
  (set! seen '())
  (void (lru 'a) (lru 'c) (lru 'd))
  (check-equal? seen '())                               ; all hits: a survived
  (void (lru 'b))
  (check-equal? seen '(b))                              ; b was the one evicted

  ;; --- clear: table and ring both reset ---
  (memo-clear! lru)
  (check-equal? (memo-size lru) 0)
  (void (lru 'a))
  (check-equal? seen '(a b))

  ;; --- the switch: off = pass straight through, entries kept warm ---
  (define swc (box 0))
  (define sw (memoize (lambda (x) (set-box! swc (add1 (unbox swc))) (* x 10))))
  (check-equal? (sw 1) 10)
  (check-equal? (unbox swc) 1)
  (memo-off! sw)
  (check-equal? (sw 1) 10)                              ; bypasses: f runs again...
  (check-equal? (sw 2) 20)
  (check-equal? (unbox swc) 3)
  (check-equal? (memo-size sw) 1)                       ; ...and nothing was filled
  (memo-on! sw)
  (check-equal? (sw 1) 10)
  (check-equal? (unbox swc) 3)                          ; the entry survived the off spell
  (check-equal? (sw 2) 20)
  (check-equal? (unbox swc) 4)                          ; uncached while off: a miss now
  (check-equal? (memo-size sw) 2)

  ;; --- #f from the key: don't cache THIS call ---
  (define fkc (box 0))
  (define fk (memoize (lambda (x) (set-box! fkc (add1 (unbox fkc))) (- x))
                      #:key (lambda (x) (and (even? x) x))))
  (check-equal? (fk 2) -2)
  (check-equal? (fk 2) -2)
  (check-equal? (unbox fkc) 1)                          ; even: cached
  (check-equal? (fk 3) -3)
  (check-equal? (fk 3) -3)
  (check-equal? (unbox fkc) 3)                          ; odd: passed through twice
  (check-equal? (memo-size fk) 1))
