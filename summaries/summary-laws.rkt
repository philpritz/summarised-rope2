#lang racket

;; Summary laws: an optional conformance kit for summary writers. Given an `smr`
;; (a variadic summary fn from `make-summary`) and a generator of domain strings,
;; the battery property-tests the monoid laws rope-core's caching/fusing/rebalancing
;; rely on but cannot check per-call:
;;   check-summary-laws  run the battery (deterministic corpus sweep + random props)
;;   law:identity        each law as a rackcheck property, runnable alone via
;;   law:associativity     (check-property [config] (law:... smr gs))
;;   law:homomorphism
;;   identity-law?       the same laws as plain predicates over concrete values:
;;   associativity-law?    (identity-law? smr x) (associativity-law? smr x y z)
;;   homomorphism-law?     (homomorphism-law? smr s cuts)
;; Depends only on the smr value (plus rackcheck/rackunit), never on rope-core, so
;; the obligations cross the module wall. Why these laws, why two groups, and why no
;; rope-integration law: scribble/summary-laws.scrbl.

(provide
 check-summary-laws
 law:identity law:associativity law:homomorphism
 identity-law? associativity-law? homomorphism-law?)

(require rackcheck rackunit racket/match)

;; ---------- the laws, as predicates ----------
(define (identity-law? smr x)                    ; unit: (smr x) = x  and  (smr x (smr)) = x
  (and (equal? (smr x) x)
       (equal? (smr x (smr)) x)))

(define (associativity-law? smr x y z)           ; x y z are summary values
  (equal? (smr (smr x y) z) (smr x (smr y z))))

;; the substrings between consecutive cut offsets; 0 and (string-length s) bracket
;; implicitly. is is sorted, repeats allowed -- a repeat yields an empty chunk,
;; exercising the unit in context.
(define (split-at-cuts s is)
  (for/list ([a (in-list (cons 0 is))]
             [b (in-list (append is (list (string-length s))))])
    (substring s a b)))

(define (homomorphism-law? smr s is)             ; measure of whole = fold of any split's measures
  (equal? (apply smr (split-at-cuts s is)) (smr s)))

;; ---------- input generators ----------
(define (gen:summary smr gs) (gen:map gs smr))    ; summary values, via measuring

(define (gen:string+cuts gs)                      ; a string and 0..6 sorted cuts
  (gen:let ([s gs]
            [is (gen:list (gen:integer-in 0 (string-length s)) #:max-length 6)])
    (list s (sort is <))))

;; ---------- the laws, as properties ----------
(define (law:identity smr gs)
  (property #:name 'identity
    ([x (gen:summary smr gs)])
    (check-true (identity-law? smr x))))

(define (law:associativity smr gs)
  (property #:name 'associativity
    ([x (gen:summary smr gs)] [y (gen:summary smr gs)] [z (gen:summary smr gs)])
    (check-true (associativity-law? smr x y z))))

(define (law:homomorphism smr gs)
  (property #:name 'homomorphism
    ([c (gen:string+cuts gs)])
    (match-define (list s is) c)
    (label! (format "~a cuts" (length is)))
    (check-true (homomorphism-law? smr s is))))

;; ---------- the battery ----------
;; Exhaustive by construction: every corpus entry is identity-checked and
;; homomorphism-checked at EVERY single cut. Multi-cut splits are left to the
;; random layer, into which check-summary-laws mixes the corpus.
(define (sweep-corpus smr corpus)
  (for ([s (in-list corpus)])
    (check-true (identity-law? smr (smr s))
                (format "identity on corpus entry ~s" s))
    (for ([i (in-range (add1 (string-length s)))])
      (check-true (homomorphism-law? smr s (list i))
                  (format "homomorphism: ~s cut at ~a" s i)))))

(define (check-summary-laws smr gs
                            #:corpus [corpus '()]
                            #:config [c (make-config)])
  (sweep-corpus smr corpus)
  (define gs* (if (null? corpus)
                  gs
                  (gen:frequency `((3 . ,gs) (1 . ,(gen:one-of corpus))))))
  (check-property c (law:identity smr gs*))
  (check-property c (law:associativity smr gs*))
  (check-property c (law:homomorphism smr gs*)))

;; ============================================================================
(module+ test
  (require "../rope-core.rkt")          ; make-summary -- a test-only dependency

  (define cc (make-summary string-length +))
  (define gs:ab (gen:string (gen:one-of (string->list "ab( )")) #:max-length 12))

  ;; --- the battery passes on a lawful summary, corpus included ---
  (check-summary-laws cc gs:ab #:corpus (list "" "a" "((b a)" "a b"))

  ;; --- the battery's teeth: the law groups catch the mutants ---
  (define minus (make-summary string-length -))   ; broken monoid; variadic seeds the fold
  (check-false (identity-law? minus (minus "a")))         ; from the FIRST arg, so a
  (check-false (associativity-law? minus (minus "") (minus "") (minus "a")))
  (check-false (homomorphism-law? minus "ab" '(1)))   ; non-monoid op now diverges from
  (check-false (homomorphism-law? minus "abc" '(1 2)))  ; the 1-ary (id-seeded) leaf too

  (define mx (make-summary string-length max))    ; fine monoid, but max-of-parts /= whole
  (check-true  (identity-law? mx (mx "a")))
  (check-true  (associativity-law? mx (mx "a") (mx "bb") (mx "c")))
  (check-false (homomorphism-law? mx "ab" '(1))))   ; -> only the string group fails
