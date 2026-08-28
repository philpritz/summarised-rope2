#lang racket

;; Sexp summary: the s-expression structure cached at every rope node.
;;   sexp-smr      ( ) [ ] { } matched by kind; value #f | sexp-val | 'malformed
;;   malformed?    a bracket-kind clash collapsed the value
;;   sand-spines   read a cut as front/back index spines (the nav interface)
;;   str-smr       naive in-string seed: a raw quote count
;;   strsexp-smr   sexp algebra gated by string state; strsexp-spines /
;;                 strsexp-in-string? read its cuts
;; Built on rope-core's `make-summary`; plain-text metrics and the `buffer-smr`
;; product live in summaries.rkt. Narrative in scribble/sexp-summary.scrbl.

(require racket/match
         "../rope-core.rkt")          ; make-summary

(provide sexp-smr malformed?
         sexp-leaf sexp+           ; the bare leaf/combine -- call directly to skip the
                                   ; make-summary dispatch when args are known raw values
         sand-spines               ; (L R) -> (values front back)
         str-smr strsexp-smr
         strsexp-spines strsexp-in-string?)

;; scratch-enabling exports (added in summarised-rope2): the guarded edge-class
;; readers the segmenting-split experiment uses. Not part of the public surface.
(module+ internal
  (provide sexp-head sexp-tail))

;; ---------- the summary value ----------
;; head/tail flank three measure fields; the stacks are signed (opens +(k+1),
;; closes -(k+1)) so a slot index reads off a stack head. Why signed: scribble.
(struct sexp-val
  (head             ; first char's class: 'open | 'close | 'ws | 'atom
   closes           ; dangling closers, innermost-first; (bracket . -(k+1))
   forms            ; complete forms at the current level
   opens            ; open brackets, innermost-first; (bracket . +(k+1))
   tail)            ; last char's class
  #:transparent)
(define (malformed? v) (eq? v 'malformed))

;; ---------- brackets ----------
(define brackets '((#\( . #\)) (#\[ . #\]) (#\{ . #\})))
(define (open?  c) (and (assv c brackets) #t))
(define (close? c) (and (memv c (map cdr brackets)) #t))
(define (matching o c) (eqv? (cdr (assv o brackets)) c))

;; ---------- from-string ----------
;; Fold the signed completion-counting algebra over the tokens, kind-strict (per-case
;; labels below). Why a frame counts at its closer, not its opener: scribble.
(define (sexp-leaf s)
  (define (tokens str)                          ; first char of each bracket / atom run
    (for/list ([t (in-list (regexp-match* #px"[][(){}]|[^][(){}\\s]+" str))]) (string-ref t 0)))
  (define (class c)
    (cond [(open? c) 'open] [(close? c) 'close] [(char-whitespace? c) 'ws] [else 'atom]))
  (and (positive? (string-length s))
       (let loop ([toks (tokens s)] [closes '()] [forms 0] [opens '()])
         (match toks
           ['() (sexp-val (class (string-ref s 0)) (reverse closes) forms opens
                          (class (string-ref s (sub1 (string-length s)))))]
           [(cons c rest)
            (define (go stack)                  ; a form completes at `stack`'s top
              (if (null? stack)
                  (loop rest closes (add1 forms) stack)
                  (loop rest closes forms (cons (cons (caar stack) (add1 (cdar stack))) (cdr stack)))))
            (cond
              [(open? c)                 (loop rest closes forms (cons (cons c 1) opens))]     ; push frame
              [(not (close? c))          (go opens)]                                            ; atom
              [(null? opens)             (loop rest (cons (cons c (- (add1 forms))) closes) 1 opens)] ; dangling closer
              [(matching (caar opens) c) (go (cdr opens))]                                      ; pop matching frame
              [else                      'malformed])]))))                                      ; kind clash

;; ---------- combine ----------
;; Cancel the right's closers against the left's opens, kind-strict; leftovers nest
;; into the left's innermost open. Mid-atom seam: `drop-start` undoes the form the
;; right's leading atom began. 'malformed absorbs; #f is the identity. Lawful: scribble.
(define (sexp+ x y)
  (define (drop-start y)
    (match y
      [(sexp-val _ (cons (cons k cv) rest) _ _ _) (struct-copy sexp-val y [closes (cons (cons k (add1 cv)) rest)])]
      [_ (struct-copy sexp-val y [forms (sub1 (sexp-val-forms y))])]))
  (define (add-inner stack n) (cons (cons (caar stack) (+ (cdar stack) n)) (cdr stack)))
  (define (merge x y)
    (match-define (sexp-val xh xc xf xo _) x)
    (match-define (sexp-val _  yc yf yo yt) y)
    (let loop ([forms xf] [stack xo] [closes yc] [out '()])
      (match closes
        [(cons (cons k cv) rest)
         (cond
           [(null? stack)             (loop 0 stack rest (cons (cons k (- cv forms)) out))] ; nothing left to cancel
           [(matching (caar stack) k) (loop forms (cdr stack) rest out)]                    ; pop, no bump
           [else                      'malformed])]                                          ; kind clash
        ['() (sexp-val xh (append xc (reverse out))
                       (if (null? stack) (+ forms yf) forms)
                       (if (null? stack) yo (append yo (if (zero? yf) stack (add-inner stack yf))))
                       yt)])))
  (cond
    [(eq? x 'malformed) 'malformed]
    [(eq? y 'malformed) 'malformed]
    [(not x) y] [(not y) x]
    [else (merge x (if (and (eq? (sexp-val-tail x) 'atom) (eq? (sexp-val-head y) 'atom)) (drop-start y) y))]))

;; ---------- the smr ----------
(define sexp-smr (make-summary sexp-leaf sexp+))

;; ---------- readers (sexp-val?-guarded: safe on #f AND 'malformed) ----------
(define (sexp-opens  v) (if (sexp-val? v) (sexp-val-opens  v) '()))
(define (sexp-closes v) (if (sexp-val? v) (sexp-val-closes v) '()))
(define (sexp-forms  v) (if (sexp-val? v) (sexp-val-forms  v) 0))
(define (sexp-head v) (and (sexp-val? v) (sexp-val-head v)))
(define (sexp-tail v) (and (sexp-val? v) (sexp-val-tail v)))

;; ---------- reading a cut as spines ----------
;; `sand-spines` reads a cut as the all-left `front` and all-right `back` spines,
;; innermost-first (slot = the entry's count); front 0-based (stored +1 drops at
;; the read), back as stored (-1 = after the last form). The ½ head refinement
;; (cut-kind) and its rationale are in scribble; the comparing algebra is in
;; sexp-edit.rkt.

;; classify a cut by the two char-classes touching it: tail of L, head of R.
;;   start  a form begins here (atom or opener)      -- flush, no lean
;;   end    right before a closer or the document end -- flush, no lean
;;   mid    straddling an atom                         -- front -½, back +½
;;   lean   whitespace; binds to the previous form     -- front -½, back -½
(define (cut-kind L R)
  (case (sexp-head R)
    [(atom)  (if (eq? (sexp-tail L) 'atom) 'mid 'start)]
    [(open)  'start]
    [(close) 'end]
    [(ws)    'lean]
    [else    'end]))                       ; R empty: the document end

;; both spines at a cut, innermost-first, ½ baked into the heads.
(define (sand-spines L R)
  (match-define (cons fh fr) (append (map (lambda (e) (sub1 (cdr e))) (sexp-opens L)) (list (sexp-forms L))))
  (match-define (cons bh br) (append (map cdr (sexp-closes R)) (list (- (add1 (sexp-forms R))))))
  (case (cut-kind L R)
    [(start end) (values (cons fh fr)        (cons bh br))]
    [(mid)       (values (cons (- fh 1/2) fr) (cons (+ bh 1/2) br))]
    [(lean)      (values (cons (- fh 1/2) fr) (cons (- bh 1/2) br))]))

;; ---------- string summary (naive) ----------
;; In-string state as a raw quote count; parity at a cut = in/out of a string. Naive
;; on purpose -- escapes, comments, char literals, |...| not discounted (scribble).
(define (str-leaf s)
  (for/sum ([c (in-string s)] #:when (char=? c #\")) 1))
(define str-smr (make-summary str-leaf +))

;; ---------- string + sexp summary (gated) ----------
;; Reuses `sexp+` gated by string state: a fragment can't know if it begins in a
;; string, so it carries the sexp-val under each entry mode (code / mid-string) and
;; the quote parity selects. Design in scribble.
(struct cs (quotes code string) #:transparent)   ; count + sexp-val-if-code + sexp-val-if-string

(define (transform s start-in-string?)            ; -> code-equivalent text: each string -> the atom "~"
  (define out (open-output-string))
  (let loop ([chs (string->list s)] [in? start-in-string?])
    (cond
      [(null? chs) (get-output-string out)]
      [(char=? (car chs) #\")
       (if in? (write-char #\space out) (write-string " ~" out))   ; close -> delimiter, open -> placeholder
       (loop (cdr chs) (not in?))]
      [in?  (loop (cdr chs) in?)]                 ; string interior: dropped
      [else (write-char (car chs) out)
            (loop (cdr chs) in?)])))

(define (strsexp-leaf s)
  (cs (for/sum ([c (in-string s)] #:when (char=? c #\")) 1)
      (sexp-leaf (transform s #f))                ; sexp-val if entered in code
      (sexp-leaf (transform s #t))))              ; sexp-val if entered in a string

(define (strsexp+ a b)
  (match-define (cs qa ca sa) a)
  (match-define (cs qb cb sb) b)
  (define flip? (odd? qa))                         ; b's entry mode flips when a holds an odd # of quotes
  (cs (+ qa qb)
      (sexp+ ca (if flip? sb cb))
      (sexp+ sa (if flip? cb sb))))

(define strsexp-smr (make-summary strsexp-leaf strsexp+))

;; cut reads: in-string = L's quote parity; spines run sand-spines on L's code val and
;; R's parity-selected val.
(define (strsexp-in-string? L) (odd? (cs-quotes L)))
(define (strsexp-spines L R)
  (sand-spines (cs-code L)
               (if (odd? (cs-quotes L)) (cs-string R) (cs-code R))))

;; ============================================================================
(module+ test
  (require rackunit)
  ;; opens/closes entries are (bracket . count); project the count for these worked
  ;; values (the bracket glyph -- the car -- isn't checked here).
  (define (opens  x) (map cdr (sexp-opens  (sexp-smr x))))
  (define (closes x) (map cdr (sexp-closes (sexp-smr x))))
  (define (forms  x) (sexp-forms  (sexp-smr x)))

  ;; --- signed, completion-counting worked values ---
  (check-equal? (opens "(")          '(1))      ; the open frame itself is +(0+1)
  (check-equal? (opens "(aa ")       '(2))
  (check-equal? (opens "(aa (p ")    '(2 2))    ; enclosing NOT bumped at the "("...
  (check-equal? (opens "(aa (p q)")  '(3))      ; ...bumped at the ")"
  (check-equal? (opens "(aa bb cc")  '(4))
  (check-equal? (closes ")")         '(-1))     ; "-1 = after last", directly in storage
  (check-equal? (forms  ")")         1)         ; the closed frame seeds the level above
  (check-equal? (closes "aa bb cc)") '(-4))
  (check-equal? (closes "q) cc)")    '(-2 -3))  ; outer entry counts the closed frame
  (check-equal? (forms  "()")        1)
  (check-equal? (opens  "()")        '())

  ;; --- the cut reads: front = (car opens) of before, back = (car closes) of after ---
  (check-equal? (car (opens  "(aa "))   2)
  (check-equal? (car (closes "bb cc)")) -3)

  ;; --- head/tail char classes ---
  (check-eq? (sexp-tail (sexp-smr "(aa bb cc")) 'atom)
  (check-eq? (sexp-head (sexp-smr "(p q) cc)")) 'open)
  (check-eq? (sexp-head (sexp-smr ") cc)")) 'close)

  ;; --- sand-spines: read a cut as front/back spines (the navigation interface) ---
  (let-values ([(front back) (sand-spines (sexp-smr "(aa ") (sexp-smr "bb cc)"))])
    (check-equal? front '(1 0))      ; ^bb: child 1 at the top, slot 0 within
    (check-equal? back  '(-3 -2)))

  ;; --- associativity: k-char pieces combine to the whole-string summary ---
  ;; (the real test of the signed completion-counting merge -- the variadic
  ;; `sexp` folds `combine` over the measured pieces, vs one straight measure)
  (define (chunked str k)
    (apply sexp-smr (for/list ([i (in-range 0 (string-length str) k)])
                  (substring str i (min (string-length str) (+ i k))))))
  (for* ([str (list "(_ _)" "((a b) c)" "(define (f x) (+ x 1))"
                    "(_ _ " "((a " ")" "a b c" "(((x)))" ") foo (bar"
                    "()" "(())" "(aa (p q) cc)" "((a b) (c d))" "x (y) z"
                    "q) cc)" "((a b) c"
                    ;; multi-bracket (now first-class) and malformed (now absorbing)
                    "([])" "[()]" "{[()]}" "(let ([x 1] [y 2]) (+ x y))" "(cond [a] [else b])"
                    "[" "])" "}])" "[)" "[(])" "(]" "([)]" "{[(])}" "[a)")]
         [k (in-range 1 6)])
    (check-equal? (chunked str k) (sexp-smr str)
                  (format "chunk size ~a of ~s" k str)))

  ;; --- multi-bracket + malformed: ( [ { are first-class, matched by kind ---
  (check-equal? (opens "[")     '(1))                              ; [ opens a level (count projected off (kind . n))
  (check-equal? (opens "(a [b") '(2 2))                            ; nested, multi-kind
  (check-equal? (sexp-smr "([])") (sexp-smr "(())"))               ; balanced: the kinds vanish, one form either way
  ;; a wrong-kind closer is a clash -> 'malformed (absorbing), wherever it occurs
  (check-true  (malformed? (sexp-smr "(a]")))
  (check-true  (malformed? (sexp-smr "[(])")))
  (check-true  (malformed? (sexp-smr "{[(])}")))
  (check-false (malformed? (sexp-smr "(a [b] c)")))               ; well-formed multi-kind is fine

  ;; --- the summary-law battery (summary-laws.rkt) on realistic generated sexps ---
  (require "summary-laws.rkt" rackcheck)

  ;; atoms: a lexicon of real words, synthesized lispy identifiers (hyphenated,
  ;; ?/!/* suffixed -- multi-char, so mid-atom cuts have targets), and numbers.
  (define gen:word    (gen:one-of '("define" "lambda" "let" "if" "cons" "x" "xs" "foo")))
  (define gen:keyword (gen:one-of '("define" "lambda" "let" "if" "+" "list" "cond")))
  (define gen:ident
    (gen:let ([c    gen:char-letter]
              [head (gen:string gen:char-letter #:max-length 3)]
              [tail (gen:list (gen:let ([c2 gen:char-letter]
                                        [s2 (gen:string gen:char-letter #:max-length 3)])
                                (string-append (string c2) s2))
                              #:max-length 2)]
              [sfx  (gen:one-of '("" "" "?" "!" "*"))])
      (string-append (string-join (cons (string-append (string c) head) tail) "-") sfx)))
  (define gen:number (gen:map gen:natural number->string))
  (define gen:atom   (gen:frequency `((4 . ,gen:word) (2 . ,gen:ident) (1 . ,gen:number))))

  ;; whitespace between siblings, drawn per junction (so shrinking simplifies
  ;; it): spaces, runs, newlines, and "" -- zero-width, legal where parens abut.
  (define gen:ws (gen:one-of '(" " " " " " "  " "\n" "\n  " "")))

  ;; trees: leaf = atom string; node = (list kids seps), keyword-headed forms
  ;; with 0..3 further children, depth-bounded so termination is structural.
  (define (gen:node max-kids d)
    (gen:let ([kids (gen:frequency
                     `((1 . ,(gen:const '()))
                       (5 . ,(gen:let ([h gen:keyword]
                                       [r (gen:list (if (zero? d)
                                                        gen:atom
                                                        (gen:tree max-kids d))
                                                    #:max-length (sub1 max-kids))])
                               (cons h r)))))]
              [seps (apply gen:tuple (make-list (max 0 (sub1 (length kids))) gen:ws))])
      (list kids seps)))
  (define (gen:tree max-kids d)
    (if (zero? d)
        gen:atom
        (gen:frequency `((2 . ,gen:atom) (2 . ,(gen:node max-kids (sub1 d)))))))

  ;; deterministic render; a zero-width separator between two ATOMS would fuse
  ;; them (changing the ground truth), so it falls back to a space there.
  (define (render t)
    (if (string? t)
        t
        (let loop ([ks (first t)] [seps (second t)] [acc ""])
          (cond
            [(null? ks)       (string-append "(" acc ")")]
            [(null? (cdr ks)) (string-append "(" acc (render (car ks)) ")")]
            [else
             (define a    (car ks))
             (define sep  (car seps))
             (define sep* (if (and (string? a) (string? (cadr ks)) (equal? sep ""))
                              " " sep))
             (loop (cdr ks) (cdr seps) (string-append acc (render a) sep*))]))))

  (define gen:sexp-doc (gen:map (gen:node 4 3) render))

  ;; structured deep-nesting docs alongside gen:sexp-doc -- the recursive shapes that stress
  ;; the spine: binary ((...)(...)) (every subtree a complete form -> empty stacks) and linear
  ;; (((...))) (a cut carries a depth-deep unclosed stack). Bounded depth keeps the strings small.
  (define (nest-binary d) (if (zero? d) "x" (string-append "(" (nest-binary (sub1 d)) " " (nest-binary (sub1 d)) ")")))
  (define (nest-linear d) (if (zero? d) "x" (string-append "(" (nest-linear (sub1 d)) ")")))
  (define gen:binary-nest (gen:map (gen:integer-in 0 5)  nest-binary))   ; <= 2^5 leaves
  (define gen:linear-nest (gen:map (gen:integer-in 0 25) nest-linear))

  ;; the same shapes as a ROPE via the DAG / rope-(b b) trick: the repeated subtree is SHARED,
  ;; so depth d is ~O(d) nodes representing 2^d leaves (a string would be 2^d chars). This is the
  ;; cheap text-size generator -- depth d => 2^d-leaf doc -- built without ever materializing it.
  (define (nest-rope smr kind d)
    (let loop ([k 0] [b ((make-rope smr) "x")])
      (if (= k d) b
          (loop (add1 k) (if (eq? kind 'binary)
                             ((make-rope smr) "(" b " " b ")")
                             ((make-rope smr) "(" b ")"))))))

  ;; curated corpus: real code, the chunk-test strings above, edge cases. Swept
  ;; deterministically (every entry, every single cut), then mixed into the
  ;; random stream. Shrunk counterexamples from failed runs get appended here.
  (define sexp-corpus
    (list "(define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))"
          "(define (map f xs) (if (null? xs) '() (cons (f (car xs)) (map f (cdr xs)))))"
          "(let loop ([i 0] [acc '()]) (if (= i 10) (reverse acc) (loop (add1 i) (cons i acc))))"
          "(lambda (x . rest) (apply + x rest))"
          "'(1 2 . 3)"
          "(display \"hello world\")"
          ";; a comment line\n(+ 1 2)"
          "(define (f g) (g))"
          "(_ _)" "((a b) c)" "(define (f x) (+ x 1))"
          "(_ _ " "((a " ")" "a b c" "(((x)))" ") foo (bar"
          "()" "(())" "(aa (p q) cc)" "((a b) (c d))" "x (y) z"
          "q) cc)" "((a b) c"
          "" " " "((((" "))))" "atom"
          ;; multi-bracket (well-formed) and malformed (now first-class)
          "([])" "[()]" "{[()]}" "(cond [(a) b] [else c])" "(f [g {h}])"
          "[)" "[(])" "([)]"))

  (check-summary-laws sexp-smr gen:sexp-doc #:corpus sexp-corpus)
  (check-summary-laws sexp-smr gen:binary-nest)
  (check-summary-laws sexp-smr gen:linear-nest)

  ;; DAG-built ropes: cached summary matches the flat string (small d, where the string is
  ;; buildable), and a huge nest builds cheaply by sharing -- 2^20 leaves in O(d), well-formed.
  (for ([d (in-range 0 7)])
    (check-equal? (sexp-smr (nest-rope sexp-smr 'binary d)) (sexp-smr (nest-binary d)) (format "binary-rope d=~a" d))
    (check-equal? (sexp-smr (nest-rope sexp-smr 'linear d)) (sexp-smr (nest-linear d)) (format "linear-rope d=~a" d)))
  (check-false (malformed? (sexp-smr (nest-rope sexp-smr 'binary 20))))    ; ~1M leaves, shared, built in O(d)
  (check-false (malformed? (sexp-smr (nest-rope sexp-smr 'linear 2000))))  ; depth-2000 spine, shared

  ;; --- gated string + sexp summary: sexp tokens inside strings are inert ---
  (define (sx str) (strsexp-smr str))
  ;; the inner "(" does not open a level -- the string is one form, like an atom `~`
  (check-equal? (cs-code (sx "(a \"(\" b")) (sexp-smr "(a ~ b"))
  (check-equal? (cs-code (sx "(\"((((\")")) (sexp-smr "(~)"))   ; depth back to 0 after a parens-full string
  ;; in-string at a cut = quote parity to the left
  (check-true  (strsexp-in-string? (sx "(a \"")))
  (check-false (strsexp-in-string? (sx "(a \"x\" ")))
  ;; spines: the string occupies its own child slot (a=child 0, string=child 1, b=child 2)
  (check-equal? (let-values ([(f b) (strsexp-spines (sx "(a \"x\" ") (sx "b)"))]) f) '(2 0))
  ;; associativity: chunked == whole, over strings full of parens (and plain sexps too)
  (define (sx-chunked str k)
    (apply strsexp-smr (for/list ([i (in-range 0 (string-length str) k)])
                         (substring str i (min (string-length str) (+ i k))))))
  (for* ([str (list "(a \"(\" b)" "\"(\"" "(\"))((\")" "x \"y z\" w"
                    "(define s \"hi (there)\")" "tail \" mid ( \" end"
                    "()" "(aa (p q) cc)" "\"unclosed (")]
         [k (in-range 1 6)])
    (check-equal? (sx-chunked str k) (strsexp-smr str)
                  (format "strsexp chunk ~a of ~s" k str)))

  ;; strsexp obeys the laws on the structured nests too (no strings inside -> tracks sexp)
  (check-summary-laws strsexp-smr gen:binary-nest)
  (check-summary-laws strsexp-smr gen:linear-nest))
