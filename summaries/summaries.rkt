#lang racket

;; Summaries: the general summary toolkit, independent of any one structure.
;;   bundle                        -- a product of summaries (the general combinator)
;;   char-smr word-smr linecol-smr -- plain-text metrics (offset, word count, line/col)
;;   buffer-smr                    -- the editor-buffer bundle: sexp navigation + the metrics
;; The summary protocol (make-summary, gen:summary-part) lives in rope-core; the sexp
;; instance in sexp-summary.rkt. This file builds on both.

(require racket/match
         "../rope-core.rkt"
         "sexp-summary.rkt")

(provide bundle
         (struct-out bundle-val)
         char-smr
         word-smr (struct-out wc)
         linecol-smr (struct-out linecol)
         hash-smr (struct-out fp)
         buffer-smr)

;; ---------- bundle: a product of summaries ----------
;; A bundle-val keys each component's value by that component's own smr, so (c bv) selects
;; c's slot via gen:summary-part. A MACRO so it can capture component source names (the
;; identifiers passed in) for display in bundle-write.

(struct bundle-val (owner slots names)  ; owner : thunk of the bundle smr that built this value
                                        ; slots : #hasheq(smr -> value) ; names : #hasheq(smr -> symbol), display-only
  #:transparent
  #:property prop:custom-write (lambda (bv port mode) (bundle-write bv port))
  #:methods gen:summary-part
  [(define (part->summary bv smr [fail (lambda ()
                                          (error 'part->summary "no ~a slot in ~v"
                                                 (object-name smr) bv))])
     ;; three askers: a component (its slot -- has-key?, because #f is a legitimate
     ;; slot value, e.g. word-smr of ""), the OWNING bundle (pass through, its combine
     ;; extracts componentwise), anything else (fail -- hash-ref's convention).
     (define slots (bundle-val-slots bv))
     (cond [(hash-has-key? slots smr)         (hash-ref slots smr)]
           [(eq? smr ((bundle-val-owner bv))) bv]
           [(procedure? fail)                 (fail)]
           [else                              fail]))])

(define (bundle-write bv port)
  (define names (bundle-val-names bv))
  (define (nm k) (hash-ref names k (lambda () (object-name k))))   ; fallback: 'smr
  (define entries
    (sort (for/list ([(k v) (in-hash (bundle-val-slots bv))]) (cons (nm k) v))
          symbol<? #:key car))                                     ; hasheq order is otherwise unstable
  (define w (apply max 0 (map (lambda (e) (string-length (symbol->string (car e)))) entries)))
  (fprintf port "(bundle")
  (for ([e (in-list entries)])
    (fprintf port "\n  [~a ~v]" (~a (car e) #:min-width w) (cdr e)))
  (fprintf port ")"))

(define (make-bundle named)             ; named : (listof (cons smr name|#f))
  (define components (map car named))
  (define names (for/hasheq ([p (in-list named)] #:when (cdr p)) (values (car p) (cdr p))))
  ;; owner is a thunk (one closure, shared by every value): make-summary calls the
  ;; leaf on "" DURING construction -- before smr is bound -- so the value can only
  ;; reference its bundle through a deferral; letrec resolves it by first call.
  (define (owner) smr)
  (define smr
    (make-summary
     (lambda (str) (bundle-val owner (for/hasheq ([c (in-list components)]) (values c (c str))) names))
     (lambda (a b)  (bundle-val owner (for/hasheq ([c (in-list components)]) (values c (c a b))) names))))
  smr)

(define-syntax (bundle stx)
  (syntax-case stx ()
    [(_ c ...)
     (with-syntax ([(named ...)
                    (map (lambda (cc) (if (identifier? cc) #`(cons #,cc '#,cc) #`(cons #,cc #f)))
                         (syntax->list #'(c ...)))])
       #'(make-bundle (list named ...)))]))

;; ---------- plain-text metrics ----------
;; Each a monoid over a text measure, read at a cut off the all-left summary. Pointwise
;; (char-smr) needs no edge state; the seam-aware ones (word, linecol) carry head/tail
;; fields so the combine can reconcile a value straddling a chunk boundary.

(define char-smr (make-summary string-length +))

(struct wc (head n tail) #:transparent)        ; head/tail: is the edge char non-whitespace? wc-n: the count
(define (word-leaf s)
  (and (positive? (string-length s))
       (wc (not (char-whitespace? (string-ref s 0)))
           (length (regexp-match* #px"\\S+" s))
           (not (char-whitespace? (string-ref s (sub1 (string-length s))))))))
(define (word+ x y)
  (or (and x y
           (match-let ([(wc xh xn xt) x] [(wc yh yn yt) y])
             (wc xh (- (+ xn yn) (if (and xt yh) 1 0)) yt)))  ; both word chars at the seam -> one word, not two
      x y))
(define word-smr (make-summary word-leaf word+))

(struct linecol (head lines cols) #:transparent)   ; head: chars before the first newline; cols: after the last (the column)
(define (linecol-leaf s)
  (define segs (regexp-split #rx"\n" s))
  (linecol (string-length (first segs))                                ; before the first newline
           (for/sum ([c (in-string s)] #:when (char=? c #\newline)) 1)
           (string-length (last segs))))                               ; after the last newline
(define (linecol+ x y)
  (match-let ([(linecol xh xl xc) x] [(linecol yh yl yc) y])
    (linecol (if (zero? xl) (+ xh yh) xh)     ; head grows until x's own first newline
             (+ xl yl)
             (if (zero? yl) (+ xc yc) yc))))   ; column grows until y's own first newline
(define linecol-smr (make-summary linecol-leaf linecol+))

;; ---------- hash-smr: the content fingerprint ----------
;; A Karp-Rabin rolling hash as a monoid: value = (fp h scale), scale = B^len mod M,
;; h(a++b) = h_a·scale_b + h_b. Associativity IS shape-blindness -- the fingerprint
;; depends only on the text, never on how the rope's branches fall, so it survives
;; rebalancing (a Merkle-style tree hash would not). Where the other metrics forget
;; the text and keep one fact, this one forgets every fact and keeps (probabilistic)
;; IDENTITY: distinct texts collide with chance ~1/M. That's what a memoized algebra
;; keys its cache by -- each char is hashed once, at its leaf; every join above is
;; O(1) arithmetic on two integer pairs.
(struct fp (h scale) #:transparent)           ; two integers: equal? is two compares
(define fp-M (- (expt 2 61) 1))               ; Mersenne prime
(define fp-B 1000003)
(define (fp-leaf s)
  (for/fold ([h 0] [sc 1] #:result (fp h sc)) ([c (in-string s)])
    (values (modulo (+ (* h fp-B) (char->integer c)) fp-M)
            (modulo (* sc fp-B) fp-M))))
(define (fp-join x y)
  (fp (modulo (+ (* (fp-h x) (fp-scale y)) (fp-h y)) fp-M)
      (modulo (* (fp-scale x) (fp-scale y)) fp-M)))
(define hash-smr (make-summary fp-leaf fp-join))

;; ---------- the buffer bundle ----------
;; sexp navigation AND the plain-text metrics in one product. Slots are keyed by smr IDENTITY
;; (eq?), so read each through THESE exported bindings -- a fresh (make-summary ...) is a
;; different object and misses the slot.
(define buffer-smr (bundle sexp-smr char-smr word-smr linecol-smr))

;; ========== EXPERIMENTAL: a guide* over linecol -- split after every newline ==========
;; Provisional, opt-in: (require (submod "summaries.rkt" experimental)). The concrete
;; instance for rope-core's experimental multisect*. Context-free, so bs/as go unused.
;; Self-contained -- delete this submodule to retract.
(module+ experimental
  (require (submod "../rope-core.rkt" experimental))
  (provide newline-guide*)

  (define (ends-nl?   v) (and (> (linecol-lines v) 0) (zero? (linecol-cols v))))  ; ends at a newline
  (define (inner-cuts v) (- (linecol-lines v) (if (ends-nl? v) 1 0)))             ; newlines not at the end

  (define newline-guide*
    (make-guide* linecol-smr
      (lambda (bs fsl fsr as)
        (define l (linecol-smr fsl))      ; normalize (bundle -> slot), as
        (define r (linecol-smr fsr))      ;   lisp-runs-guide* does
        (values (> (inner-cuts l) 0)      ; left?  -- a cut inside the left child
                (ends-nl? l)              ; mid?   -- seam sits right after a newline
                (> (inner-cuts r) 0)))))  ; right? -- a cut inside the right child

  (module+ test
    (require rackunit)
    (define build (make-rope linecol-smr))
    (define (lines s) (map (lambda (p) (format "~a" p)) ((multisect* newline-guide*) (build s))))
    (check-equal? (lines "a\nb\nc")    '("a\n" "b\n" "c"))
    (check-equal? (lines "abc")        '("abc"))
    (check-equal? (lines "a\n")        '("a\n"))            ; trailing newline -> no empty final piece
    (check-equal? (lines "\nabc")      '("\n" "abc"))
    (check-equal? (lines "x\ny z\nw\n")'("x\n" "y z\n" "w\n"))
    ;; a branched rope (> max-leaf), so the prune path runs
    (check-equal? (lines (string-append (make-string 40 #\a) "\n" (make-string 40 #\b)))
                  (list (string-append (make-string 40 #\a) "\n") (make-string 40 #\b)))))

(module+ test
  (require rackunit)

  (let* ([cc (make-summary string-length +)]
         [b  (bundle sexp-smr cc)])
    (check-equal? (sexp-smr (b "(aa bb")) (sexp-smr "(aa bb"))
    (check-equal? (cc (b "(aa bb")) 6)
    (check-equal? (b "(aa" " bb") (b "(aa bb")))

  ;; --- part->summary: the three askers ---
  (let* ([cc  (make-summary string-length +)]
         [b   (bundle word-smr cc)]
         [big (bundle word-smr cc linecol-smr)]
         [v   (b "one two")])
    ;; a component: its slot -- including a #f-valued one (word-smr of "" is #f)
    (check-equal? (cc v) 7)
    (check-false  (word-smr (b "")))
    ;; the OWNING bundle: passes through -- normalization and self-combining work
    (check-equal? (b v) v)
    (check-equal? (b v (b " three")) (b "one two three"))
    ;; anything else fails: a foreign leaf smr, and a SUB-bundle of a bigger value
    ;; (projection is no longer silent -- ask with the value's own algebra)
    (check-exn #rx"no .* slot" (lambda () (linecol-smr v)))
    (check-exn #rx"no .* slot" (lambda () (b (big "one two"))))
    ;; the fail argument, hash-ref's convention: a value, or a thunk to call
    (check-equal? (part->summary v linecol-smr 'absent) 'absent)
    (check-equal? (part->summary v linecol-smr (lambda () 'computed)) 'computed))

  (check-equal? (char-smr "hello") 5)
  (define (count-where pred)
    (make-summary (lambda (s) (for/sum ([c (in-string s)] #:when (pred c)) 1)) +))
  (check-equal? ((count-where char-whitespace?) "a b  c") 3)

  (define (word-count s) (cond [(word-smr s) => wc-n] [else 0]))
  (check-equal? (word-count "the quick brown fox") 4)
  (check-equal? (word-count "  ") 0)
  (check-equal? (word-count "")  0)
  (check-equal? (word-smr "hel" "lo") (word-smr "hello"))
  (check-equal? (word-smr "a b" " c")  (word-smr "a b c"))

  (define (line/col s i)
    (let ([v (linecol-smr (substring s 0 i))]) (cons (linecol-lines v) (linecol-cols v))))
  (check-equal? (line/col "ab\ncd\nef" 0) '(0 . 0))
  (check-equal? (line/col "ab\ncd\nef" 4) '(1 . 1))
  (check-equal? (line/col "ab\ncd\nef" 6) '(2 . 0))
  (check-equal? (linecol-head (linecol-smr "ab\ncd\nef")) 2)   ; "ab" before the first newline
  (check-equal? (linecol-head (linecol-smr "abc"))       3)    ; no newline -> head is the whole string
  (check-equal? (linecol-smr "ab\nc" "d\nef") (linecol-smr "ab\ncd\nef"))

  ;; --- hash-smr: content identity, shape-blind ---
  (check-equal? (hash-smr "ab" "cd") (hash-smr "abcd"))
  (check-false  (equal? (hash-smr "ab") (hash-smr "ba")))      ; order distinguishes
  (check-equal? (hash-smr "") (fp 0 1))                        ; the identity
  (check-equal? (hash-smr ((make-rope hash-smr) "(a \"x\ny\")"))
                (hash-smr "(a \"x\ny\")"))                     ; a rope hashes as its text

  (require "summary-laws.rkt" rackcheck)
  (define gen:text (gen:string (gen:one-of (string->list "ab  \n()")) #:max-length 16))
  (define metric-corpus (list "" " " "a" "ab cd" "a\nb\n" "\n\n" "  ab  " "x\ny z\nw"))
  (check-summary-laws char-smr    gen:text #:corpus metric-corpus)
  (check-summary-laws word-smr    gen:text #:corpus metric-corpus)
  (check-summary-laws linecol-smr gen:text #:corpus metric-corpus)
  (check-summary-laws hash-smr    gen:text #:corpus metric-corpus)

  (let ([v (buffer-smr "(define x\ny)")])
    (check-equal? (char-smr v) 12)
    (check-equal? (wc-n (word-smr v)) 3)
    (check-equal? (linecol-lines (linecol-smr v)) 1)
    (check-equal? (linecol-cols  (linecol-smr v)) 2)
    (check-equal? (sexp-smr v) (sexp-smr "(define x\ny)")))

  (let ([r ((make-rope buffer-smr) "(define x\ny)")])
    (check-equal? (char-smr r) 12)
    (check-equal? (wc-n (word-smr r)) 3)
    (check-equal? (sexp-smr r) (sexp-smr "(define x\ny)"))))
