#lang racket

;; Deque: a persistent double-ended queue as two lists -- `front` held front-first,
;; `rear` held rear-first; the sequence reads (append front (reverse rear)). Pushes
;; and pops at both ends are O(1), except a pop whose own side is empty: the other
;; side splits in half, one half reversing across (O(n) that amortizes away under
;; one-directional use; under heavy persistence the Okasaki banker's deque is the
;; upgrade -- deliberately skipped at window scale). equal? is representational:
;; the same sequence under different splits compares unequal -- compare through
;; deque->list.
;;
;; The end operations are ISOS between (x . deque) and the non-empty deques: each
;; push is the inverse of its pop and vice versa, callable as plain functions --
;; (push-front x d) -> d*, (pop-front d) -> (values x rest) -- and composable with
;; compose-iso/expt-iso ((compose-iso push-rear pop-front) is rotate-toward-rear).
;; Partial on the empty deque (the pops error), and lawful modulo representation:
;; a pop that rebalanced re-pushes to the same SEQUENCE under a different split.
;; iso-law? cannot check them (it round-trips one value; these carry two).

(require "algebra.rkt")   ; iso, inverse-iso

(provide deque                    ; (deque x ...) -> the deque of the x, front-first
         deque? empty-deque empty-deque?
         push-front push-rear     ; isos: x deque -> deque
         pop-front pop-rear       ; their inverses: deque -> (values x rest); error on empty
         deque-first deque-last   ; peeks; error on empty
         deque-length deque->list list->deque)

(struct dq (front rear) #:transparent)

(define deque?      dq?)
(define empty-deque (dq '() '()))
(define (empty-deque? d) (and (null? (dq-front d)) (null? (dq-rear d))))

(define (list->deque xs) (dq xs '()))
(define (deque . xs)     (list->deque xs))
(define (deque->list d)  (append (dq-front d) (reverse (dq-rear d))))
(define (deque-length d) (+ (length (dq-front d)) (length (dq-rear d))))

(define (push-front* x d) (dq (cons x (dq-front d)) (dq-rear d)))
(define (push-rear*  x d) (dq (dq-front d) (cons x (dq-rear d))))

;; a pop on an empty side: split the other side, half reversing across. The kept
;; half is the near half of that side, so alternating end-pops split work evenly.
(define (pop-front* d)
  (match d
    [(dq (cons x f) r) (values x (dq f r))]
    [(dq '() '())      (error 'pop-front "empty deque")]
    [(dq '() r)
     (define-values (keep give) (split-at r (quotient (length r) 2)))
     (pop-front* (dq (reverse give) keep))]))

(define (pop-rear* d)
  (match d
    [(dq f (cons x r)) (values x (dq f r))]
    [(dq '() '())      (error 'pop-rear "empty deque")]
    [(dq f '())
     (define-values (keep give) (split-at f (quotient (length f) 2)))
     (pop-rear* (dq keep (reverse give)))]))

;; each push worn as an iso; each pop is literally its inverse (the from/to swap)
(define push-front (iso push-front* pop-front*))
(define pop-front  (inverse-iso push-front))
(define push-rear  (iso push-rear* pop-rear*))
(define pop-rear   (inverse-iso push-rear))

(define (deque-first d)
  (match d
    [(dq (cons x _) _) x]
    [(dq '() '())      (error 'deque-first "empty deque")]
    [(dq '() r)        (last r)]))

(define (deque-last d)
  (match d
    [(dq _ (cons x _)) x]
    [(dq '() '())      (error 'deque-last "empty deque")]
    [(dq f '())        (last f)]))

;; ============================================================================
(module+ test
  (require rackunit)

  (define d (deque 1 2 3 4))

  ;; --- reads ---
  (check-equal? (deque->list d) '(1 2 3 4))
  (check-equal? (deque-length d) 4)
  (check-equal? (deque-first d) 1)
  (check-equal? (deque-last d)  4)
  (check-true  (empty-deque? empty-deque))
  (check-false (empty-deque? d))
  (check-equal? (deque->list (list->deque '(a b))) '(a b))

  ;; --- the four end operations ---
  (check-equal? (deque->list (push-front 0 d)) '(0 1 2 3 4))
  (check-equal? (deque->list (push-rear  5 d)) '(1 2 3 4 5))
  (let-values ([(x rest) (pop-front d)])
    (check-equal? x 1)
    (check-equal? (deque->list rest) '(2 3 4)))
  (let-values ([(x rest) (pop-rear d)])
    (check-equal? x 4)
    (check-equal? (deque->list rest) '(1 2 3)))

  ;; --- persistence: pops leave the original untouched ---
  (let-values ([(_x _r) (pop-front d)])
    (check-equal? (deque->list d) '(1 2 3 4)))

  ;; --- the rebalance paths: pops against the grain ---
  (let ([r-only (push-rear 3 (push-rear 2 (push-rear 1 empty-deque)))])
    (let*-values ([(a d1) (pop-front r-only)]     ; front empty: split crosses
                  [(b d2) (pop-front d1)]
                  [(c d3) (pop-front d2)])
      (check-equal? (list a b c) '(1 2 3))
      (check-true (empty-deque? d3))))
  (let ([f-only (push-front 1 (push-front 2 (push-front 3 empty-deque)))])
    (let*-values ([(a d1) (pop-rear f-only)]      ; rear empty: mirror
                  [(b d2) (pop-rear d1)]
                  [(c d3) (pop-rear d2)])
      (check-equal? (list a b c) '(3 2 1))
      (check-true (empty-deque? d3))))

  ;; --- the sliding-window pattern: push one end, pop the other, both ways ---
  (check-equal? (let loop ([d (deque 1 2 3)] [i 4] [acc '()])
                  (cond [(> i 9) (append (reverse acc) (deque->list d))]
                        [else (define-values (x rest) (pop-front (push-rear i d)))
                              (loop rest (add1 i) (cons x acc))]))
                '(1 2 3 4 5 6 7 8 9))
  (check-equal? (let*-values ([(d) (deque 1 2 3 4 5 6)]        ; alternate ends
                              [(a d) (pop-front d)] [(b d) (pop-rear d)]
                              [(c d) (pop-front d)] [(e d) (pop-rear d)])
                  (list a b c e (deque->list d)))
                '(1 6 2 5 (3 4)))

  ;; --- peeks across the representation seam ---
  (let ([r-only (push-rear 2 (push-rear 1 empty-deque))])
    (check-equal? (deque-first r-only) 1)
    (check-equal? (deque-last  r-only) 2))

  ;; --- errors on empty ---
  (check-exn #rx"empty deque" (lambda () (pop-front empty-deque)))
  (check-exn #rx"empty deque" (lambda () (pop-rear empty-deque)))
  (check-exn #rx"empty deque" (lambda () (deque-first empty-deque)))
  (check-exn #rx"empty deque" (lambda () (deque-last empty-deque)))

  ;; --- the ops ARE isos: each pair mutually inverse, composable ---
  (check-eq? (iso-to push-front) (iso-from pop-front))
  (check-eq? (iso-from push-front) (iso-to pop-front))
  ;; round trip through composition, on the values channel (no rebalance: exact)
  (check-equal? (call-with-values (lambda () ((compose-iso pop-front push-front) 9 d)) list)
                (list 9 d))
  ;; rotate = pop one end into a push at the other, as ONE composed iso
  (let ([rot (compose-iso push-rear pop-front)])
    (check-equal? (deque->list (rot (deque 1 2 3)))               '(2 3 1))
    (check-equal? (deque->list ((expt-iso rot 3) (deque 1 2 3)))  '(1 2 3))   ; full cycle
    (check-equal? (deque->list ((expt-iso rot -1) (deque 1 2 3))) '(3 1 2)))) ; inverse rotates back
