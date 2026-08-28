#lang racket/base

;; lru.rkt -- a mutable LRU map: hash lookup + a recency ring, capped.
;; Split out of memoize.rkt (whose engine comment anticipated "a second
;; consumer" -- trie.rkt is that consumer; memoize now rides this too).
;;   (make-lru cap #:on-evict h)  cap: max live entries (+inf.0 = uncapped);
;;                                h: (k v -> void), fires AFTER a cap eviction
;;   (lru-ref  l k fail)   promoting read: a hit moves the entry to the front
;;   (lru-peek l k fail)   read without promoting -- for observers (walks)
;;   (lru-set! l k v)      insert/update + promote; past cap the least-recently-
;;                         USED entry is dropped (working set survives pressure)
;;   (lru-ref! l k make)   get, or (make)+insert, promoting -- hash-ref! style
;;   (lru-remove! l k)     explicit removal; NOT an eviction, the hook stays quiet
;;   (lru-walk l proc)     (proc k v) per entry, most-recent first, no promotion
;; fail follows the hash-ref convention: returned as-is on a miss, or called if
;; a thunk; #f is a perfectly storable value. An IMPERATIVE POCKET: the engine
;; mutates; nothing mutable escapes.

(provide make-lru                ; cap #:on-evict -> an empty lru
         lru?                    ; recognizes one
         lru-ref                 ; l k fail -> v (promoting)
         lru-peek                ; l k fail -> v (order untouched)
         lru-set!                ; l k v    -> void (may evict the tail)
         lru-ref!                ; l k make -> v (get or create+insert)
         lru-remove!             ; l k      -> void (no hook)
         lru-count               ; live entries
         lru-walk                ; l proc   -> void, MRU-first
         lru-clear!)             ; drop every entry

;; ---------- the engine: a sentinel-ring doubly linked list (PRIVATE) ----------
;; The sentinel's next is the head, its prev the tail; empty = the sentinel
;; linked to itself, so every operation is an unconditional splice. Every node
;; is its own HANDLE: unlink!/move-front! are O(1) -- the whole point (the LRU
;; hit path).
(struct node ([val #:mutable] [prev #:mutable] [next #:mutable]))

(define (make-dll)
  (define s (node 'sentinel #f #f))
  (set-node-prev! s s) (set-node-next! s s)
  s)

(define (splice! a n b)                       ; link a <-> n <-> b
  (set-node-next! a n) (set-node-prev! n a)
  (set-node-next! n b) (set-node-prev! b n))

(define (dll-push-front! s v) (define n (node v #f #f)) (splice! s n (node-next s)) n)
(define (dll-unlink! n)
  (set-node-next! (node-prev n) (node-next n))
  (set-node-prev! (node-next n) (node-prev n))
  n)
(define (dll-move-front! s n) (splice! s (dll-unlink! n) (node-next s)) n)
(define (dll-empty? s)    (eq? (node-next s) s))
(define (dll-pop-tail! s) (and (not (dll-empty? s)) (dll-unlink! (node-prev s))))

;; ---------- the lru map ----------
;; table maps key -> ring node; a node's val is (key . value), the key riding
;; along so eviction can drop its table entry. Hit = move-to-front; insert =
;; push, evict the tail past cap. All O(1).
(struct cache (cap table ring on-evict))

(define (make-lru cap #:on-evict [on-evict void])
  (cache cap (make-hash) (make-dll) on-evict))
(define lru? cache?)

(define (miss fail) (if (procedure? fail) (fail) fail))

(define (lru-ref l k fail)
  (define n (hash-ref (cache-table l) k #f))
  (cond [n (dll-move-front! (cache-ring l) n) (cdr (node-val n))]
        [else (miss fail)]))

(define (lru-peek l k fail)
  (define n (hash-ref (cache-table l) k #f))
  (if n (cdr (node-val n)) (miss fail)))

(define (lru-set! l k v)
  (define t (cache-table l))
  (define n (hash-ref t k #f))
  (cond [n (set-node-val! n (cons k v)) (dll-move-front! (cache-ring l) n)]
        [else
         (hash-set! t k (dll-push-front! (cache-ring l) (cons k v)))
         (when (> (hash-count t) (cache-cap l))
           (define dead (node-val (dll-pop-tail! (cache-ring l))))
           (hash-remove! t (car dead))
           ((cache-on-evict l) (car dead) (cdr dead)))])
  (void))

(define missing (string->uninterned-symbol "missing"))
(define (lru-ref! l k make)
  (define v (lru-ref l k missing))
  (if (eq? v missing) (let ([v (make)]) (lru-set! l k v) v) v))

(define (lru-remove! l k)
  (define n (hash-ref (cache-table l) k #f))
  (when n (dll-unlink! n) (hash-remove! (cache-table l) k))
  (void))

(define (lru-count l) (hash-count (cache-table l)))

(define (lru-walk l proc)                     ; proc must not mutate l mid-walk
  (define s (cache-ring l))
  (let loop ([n (node-next s)])
    (unless (eq? n s)
      (proc (car (node-val n)) (cdr (node-val n)))
      (loop (node-next n)))))

(define (lru-clear! l)
  (hash-clear! (cache-table l))
  (define s (cache-ring l))
  (set-node-prev! s s) (set-node-next! s s))

;; ============================================================================
(module+ test
  (require rackunit)

  ;; --- the engine, white-box: order, handles, eviction path ---
  (define d (make-dll))
  (check-true (dll-empty? d))
  (check-false (dll-pop-tail! d))                       ; popping empty: #f, no error
  (define a (dll-push-front! d 'a))
  (define b (dll-push-front! d 'b))
  (define c (dll-push-front! d 'c))
  (define (order s) (let loop ([n (node-next s)])
                      (if (eq? n s) '() (cons (node-val n) (loop (node-next n))))))
  (check-equal? (order d) '(c b a))                     ; fronts stack
  (void (dll-move-front! d a))                          ; the hit path
  (check-equal? (order d) '(a c b))
  (void (dll-move-front! d a))                          ; already front: stable
  (check-equal? (order d) '(a c b))
  (check-eq? (node-val (dll-pop-tail! d)) 'b)           ; the eviction path
  (void (dll-unlink! c))                                ; unlink from the middle
  (check-equal? (order d) '(a))

  ;; --- ref / peek / set!: hits promote, peeks don't, #f stores fine ---
  (define l (make-lru 3))
  (lru-set! l 'x 1) (lru-set! l 'y #f) (lru-set! l 'z 3)
  (check-equal? (lru-ref l 'x 'MISS) 1)
  (check-false  (lru-ref l 'y 'MISS))                   ; stored #f, not a miss
  (check-equal? (lru-ref l 'q 'MISS) 'MISS)             ; miss value returned as-is
  (check-equal? (lru-ref l 'q (lambda () 'called)) 'called)   ; thunk fail is called
  (check-equal? (lru-count l) 3)
  ;; walk order is recency, MRU first: the refs above promoted y then x over z
  (void (lru-ref l 'x 'MISS))
  (define (keys) (let ([ks '()]) (lru-walk l (lambda (k v) (set! ks (cons k ks)))) (reverse ks)))
  (check-equal? (keys) '(x y z))
  (void (lru-peek l 'z 'MISS))                          ; peek: order untouched
  (check-equal? (keys) '(x y z))

  ;; --- eviction: LRU dies, the working set survives; the hook sees the drop ---
  (define dropped '())
  (define e (make-lru 2 #:on-evict (lambda (k v) (set! dropped (cons (cons k v) dropped)))))
  (lru-set! e 'a 1) (lru-set! e 'b 2)
  (void (lru-ref e 'a 'MISS))                           ; touch a: b is now LRU
  (lru-set! e 'c 3)                                     ; over cap: evicts b, not a
  (check-equal? (lru-ref e 'a 'MISS) 1)
  (check-equal? (lru-ref e 'b 'MISS) 'MISS)
  (check-equal? dropped '((b . 2)))
  ;; updating an existing key is not an insert: no eviction
  (lru-set! e 'a 10)
  (check-equal? (lru-count e) 2)
  (check-equal? (lru-ref e 'a 'MISS) 10)
  ;; remove! is not an eviction: the hook stays quiet
  (lru-remove! e 'c)
  (check-equal? (lru-count e) 1)
  (check-equal? dropped '((b . 2)))
  (lru-remove! e 'ghost)                                ; removing a miss: no-op

  ;; --- ref!: get-or-create, the memo hit/miss path ---
  (define made (box 0))
  (define m (make-lru 8))
  (define (get) (lru-ref! m 'k (lambda () (set-box! made (add1 (unbox made))) 'v)))
  (check-equal? (get) 'v)
  (check-equal? (get) 'v)
  (check-equal? (unbox made) 1)                         ; second call hit

  ;; --- uncapped: +inf.0 never trips ---
  (define u (make-lru +inf.0))
  (for ([i (in-range 100)]) (lru-set! u i i))
  (check-equal? (lru-count u) 100)

  ;; --- clear!: table and ring both reset ---
  (lru-clear! l)
  (check-equal? (lru-count l) 0)
  (check-equal? (keys) '())
  (lru-set! l 'fresh 1)                                 ; usable after clear
  (check-equal? (lru-ref l 'fresh 'MISS) 1))
