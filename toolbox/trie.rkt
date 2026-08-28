#lang racket/base

;; trie.rkt -- a mutable trie of nested LRU maps, variadic in keys.
;;
;;   (define-values (put! ref clear! walk) (make-trie #:caps (stream* 100 (forever 10))))
;;
;; A node is a value slot + an lru of child edges (lru.rkt), so the trie models
;; a lazy, (implicitly) infinite structure: a cell for every possible key-path,
;; only the touched paths materialized, and the fan-out at every level BOUNDED.
;; The key is given VARIADICALLY -- its steps are the path down the trie.
;;   (put! v k ...)      store v at path k ...
;;   (ref fail k ...)    value at k ..., else fail (called if a thunk) [hash-ref style]
;;   (clear! k ...)      drop value at k ..., pruning emptied nodes upward
;;   (walk proc k ...)   (proc full-path value) for every value at/under prefix k ...
;;
;; #:caps is a STREAM of per-depth fan-out caps, PEELED down the tree: a node
;; takes the head as its own child cap and hands the tail to its children --
;; no depth counting anywhere. (stream* 32 (forever 8)) reads "32 edges at the
;; root, 8 per node below". An exhausted (finite) stream means uncapped from
;; there on. Caps bound FAN-OUT, not total size: n levels of cap m admit up to
;; m^n values. A node's own value rides free -- only child edges count.
;; A LIST of caps (every list is a stream) additionally FIXES the key depth:
;; (make-trie #:caps '(64 128)) is a two-level map, and put!/ref/clear! insist
;; on exactly two key steps (a shorter or longer key is an error, not a miss).
;; walk still takes any prefix up to that depth.
;;
;; The LRU discipline, hierarchically: put!/ref promote every edge on their
;; path (deep use keeps ancestors alive -- an edge goes LRU only when nothing
;; under it is being used); clear! and walk are observers and promote nothing.
;; A node over its cap evicts its least-recently-used EDGE -- the whole subtree
;; under it drops, GC reclaiming the nodes (each owns its own ring; no teardown).
;;
;; GC-correctness discipline (inherited from the plain-hash generation):
;;   - only put! may CREATE levels;
;;   - ref/clear!/walk descend WITHOUT creating, else a read resurrects the
;;     empty shell clear! just pruned;
;;   - clear! prunes any node left dead (no value, no children) on the way up.
;; An IMPERATIVE POCKET: the nodes mutate; nothing mutable escapes.

(require racket/stream "lru.rkt")

(provide make-trie               ; #:caps -> (values put! ref clear! walk)
         forever)                ; v -> the constant infinite stream of v

(define (forever v) (stream-cons v (forever v)))

(define none (string->uninterned-symbol "none"))   ; private: "no value at this node"
(struct tnode ([val #:mutable] kids tail))         ; tail: the caps for the children

(define (make-trie #:caps [caps (forever +inf.0)])
  (define depth (and (list? caps) (length caps)))  ; a list fixes the key depth
  (define (check who ks)
    (when (and depth (not (= (length ks) depth)))
      (error who "key depth: expected ~a steps, got ~a: ~e" depth (length ks) ks)))
  (define (new-node caps)
    (if (stream-empty? caps)                       ; exhausted stream: uncapped below
        (tnode none (make-lru +inf.0) caps)
        (tnode none (make-lru (stream-first caps)) (stream-rest caps))))
  (define root (new-node caps))

  (define (dig n ks)                               ; creating descent; promotes each edge
    (if (null? ks) n
        (dig (lru-ref! (tnode-kids n) (car ks)
                       (lambda () (new-node (tnode-tail n))))
             (cdr ks))))

  (define (find n ks touch)                        ; non-creating; touch: lru-ref | lru-peek
    (cond [(null? ks) n]
          [else (define sub (touch (tnode-kids n) (car ks) #f))
                (and sub (find sub (cdr ks) touch))]))

  (define (put! v . ks)
    (check 'trie-put! ks)
    (set-tnode-val! (dig root ks) v))

  (define (ref fail . ks)
    (check 'trie-ref ks)
    (define n (find root ks lru-ref))
    (if (and n (not (eq? (tnode-val n) none)))
        (tnode-val n)
        (if (procedure? fail) (fail) fail)))

  (define (clear! . ks)                            ; removal is not use: peek descent
    (check 'trie-clear! ks)
    (let loop ([n root] [ks ks])
      (cond [(null? ks) (set-tnode-val! n none)]
            [else (define sub (lru-peek (tnode-kids n) (car ks) #f))
                  (when sub
                    (loop sub (cdr ks))
                    (when (and (eq? (tnode-val sub) none)          ; dead: no value,
                               (zero? (lru-count (tnode-kids sub))))  ; no children
                      (lru-remove! (tnode-kids n) (car ks))))]))
    (void))

  ;; visit every stored (full-path value) at or under the prefix; per level the
  ;; children come most-recently-used first (lru-walk's order)
  (define (walk proc . prefix)
    (when (and depth (> (length prefix) depth))
      (error 'trie-walk "key depth: prefix deeper than ~a: ~e" depth prefix))
    (define start (find root prefix lru-peek))
    (when start
      (let recur ([n start] [rev (reverse prefix)])
        (unless (eq? (tnode-val n) none) (proc (reverse rev) (tnode-val n)))
        (lru-walk (tnode-kids n) (lambda (k sub) (recur sub (cons k rev)))))))

  (values put! ref clear! walk))

;; ============================================================================
(module+ test
  (require rackunit racket/list racket/format)

  (define (contents walk . prefix)                 ; walk -> assoc list, path -> value
    (define acc '())
    (apply walk (lambda (p v) (set! acc (cons (cons p v) acc))) prefix)
    (reverse acc))

  ;; --- the four commands, uncapped: store, hit, miss, stored #f, prefix walk ---
  (define-values (put! ref clear! walk) (make-trie))
  (put! 1 'a 'b 'c)
  (put! 2 'a 'b 'd)
  (put! #f 'a)                                     ; #f is a real stored value
  (check-equal? (ref 'MISS 'a 'b 'c) 1)
  (check-false  (ref 'MISS 'a))                    ; stored, not a miss
  (check-equal? (ref 'MISS 'a 'b) 'MISS)           ; interior node, no value
  (check-equal? (ref 'MISS 'a 'b 'z) 'MISS)
  (check-equal? (ref (lambda () 'called) 'q) 'called)   ; thunk fail is called
  (check-equal? (sort (map car (contents walk 'a 'b)) string<? #:key ~a)
                '((a b c) (a b d)))                ; everything under the prefix
  (check-equal? (length (contents walk)) 3)        ; ... and from the root, all three

  ;; --- clear! prunes dead spines, stops at live forks (the cat/car/dog drill) ---
  (define-values (cput cref cclear cwalk) (make-trie))
  (cput 1 #\c #\a #\t)
  (cput 2 #\c #\a #\r)                             ; shares "ca"
  (cput 3 #\d #\o #\g)                             ; its own spine
  (cclear #\d #\o #\g)                             ; unique spine: rolls up entirely
  (check-equal? (length (contents cwalk)) 2)
  (cclear #\c #\a #\t)                             ; prune stops at "ca": car survives
  (check-equal? (cref 'GONE #\c #\a #\r) 2)
  (check-equal? (cref 'GONE #\c #\a #\t) 'GONE)
  ;; clearing an interior value doesn't take the children with it
  (define-values (iput iref iclear iwalk) (make-trie))
  (iput 'top 'x)
  (iput 'deep 'x 'y)
  (iclear 'x)
  (check-equal? (iref 'GONE 'x) 'GONE)
  (check-equal? (iref 'GONE 'x 'y) 'deep)
  ;; clearing a miss is a quiet no-op
  (iclear 'never 'was)

  ;; --- caps peel per level: the root takes the head, children the tail ---
  (define-values (pput pref pclear pwalk) (make-trie #:caps (stream* 1 (forever 2))))
  (pput 'ax 'a 'x)
  (pput 'by 'b 'y)                                 ; root cap 1: edge a evicted, subtree gone
  (check-equal? (pref 'GONE 'a 'x) 'GONE)
  (check-equal? (pref 'GONE 'b 'y) 'by)
  (pput 'b1 'b 1) (pput 'b2 'b 2)                  ; level-1 cap is 2; y b1 b2 -> y evicted
  (check-equal? (pref 'GONE 'b 'y) 'GONE)
  (check-equal? (pref 'GONE 'b 1) 'b1)
  (check-equal? (pref 'GONE 'b 2) 'b2)

  ;; --- recency propagates up: deep use keeps the ancestor edge alive ---
  (define-values (rput rref rclear rwalk) (make-trie #:caps (stream* 2 (forever +inf.0))))
  (rput 1 'a 'x)
  (rput 2 'b 'y)
  (void (rref 'MISS 'a 'x))                        ; touch a's subtree: b is now LRU at the root
  (rput 3 'c 'z)                                   ; root over cap: b dies, a survives
  (check-equal? (rref 'MISS 'a 'x) 1)
  (check-equal? (rref 'MISS 'b 'y) 'MISS)
  ;; ... and walk/clear! are observers: a peek-heavy walk scrambles nothing
  (define-values (wput wref wclear wwalk) (make-trie #:caps (stream 2)))
  (wput 1 'old) (wput 2 'new)
  (void (contents wwalk))                          ; enumerate: must not promote
  (void (wref 'MISS 'new))
  (wput 3 'newest)                                 ; old is still the LRU: it dies
  (check-equal? (wref 'MISS 'old) 'MISS)
  (check-equal? (wref 'MISS 'new) 2)

  ;; --- a finite stream, exhausted, means uncapped from there down ---
  (define-values (fput fref fclear fwalk) (make-trie #:caps (stream 2)))
  (for ([i (in-range 10)]) (fput i 'k i))          ; depth 2: past the stream's end
  (check-equal? (length (contents fwalk 'k)) 10)   ; all ten kept: no cap below level 0

  ;; --- LIST caps fix the key depth: exactly that many steps, checked ---
  (define-values (dput dref dclear dwalk) (make-trie #:caps '(4 4)))
  (dput 'v "needle" 'cell)                         ; two caps -> two-step keys
  (check-equal? (dref 'MISS "needle" 'cell) 'v)
  (check-exn #rx"key depth" (lambda () (dput 'v "needle")))          ; too shallow
  (check-exn #rx"key depth" (lambda () (dput 'v "needle" 'cell 'x))) ; too deep
  (check-exn #rx"key depth" (lambda () (dref 'MISS "needle")))
  (check-exn #rx"key depth" (lambda () (dclear "needle")))
  (check-exn #rx"key depth" (lambda () (dwalk void 'a 'b 'c)))       ; prefix past the bottom
  (check-equal? (contents dwalk "needle") '((("needle" cell) . v)))  ; prefix <= depth: fine
  (check-equal? (contents dwalk) '((("needle" cell) . v)))
  ;; the caps still cap: level-1 fan-out 4 under one word
  (for ([i (in-range 6)]) (dput i "word" i))
  (check-equal? (length (contents dwalk "word")) 4)
  ;; a stream of the same caps enforces nothing (lists opt in, streams don't)
  (define-values (sput sref sclear swalk) (make-trie #:caps (stream 4 4)))
  (sput 'shallow 'k)                               ; one step: fine under a stream
  (check-equal? (sref 'MISS 'k) 'shallow)

  ;; --- the value slot rides free: a node's own value is not a child edge ---
  (define-values (vput vref vclear vwalk) (make-trie #:caps (forever 2)))
  (vput 'own 'a)
  (vput 1 'a 'p) (vput 2 'a 'q)                    ; two edges under a: exactly at cap
  (check-equal? (vref 'MISS 'a) 'own)              ; the value did not count against it
  (check-equal? (vref 'MISS 'a 'p) 1)
  (check-equal? (vref 'MISS 'a 'q) 2))
