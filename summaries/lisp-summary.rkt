#lang racket

;; Lisp summary: the sexp structure of real Lisp source -- the sexp algebra gated by
;; Lisp lexical syntax: string literals (backslash escapes honoured), `;` line
;; comments, `#| |#` block comments (non-nesting), and `#\` char literals. Brackets
;; and quotes inside any of them are inert.
;;   lisp-smr          the gated summary
;;   lisp-state        entry mode at a cut: 'code | 'string | 'escape | 'comment
;;                                        | 'hash | 'charlit | 'block | 'block-pipe
;;   lisp-in-string?   lisp-in-comment?    the state, as predicates
;;   lisp-spines       sand-spines on the mode-selected sexp values
;;   class-sides       a cut as the pair of RUN CLASSES touching it (sand-spines'
;;                     shape for the lexical layer); runs feed the experimental
;;                     lisp-runs-guide* -- multisect* into render pieces.
;; The gate is a lexer mode machine. A fragment cannot know its entry mode -- that
;; depends on text to its left -- so the value carries, for EVERY entry mode, an arm:
;; the exit mode, the sexp measure of the fragment read under that entry, and the
;; run fields below; combine threads the left's exit into the right's entry
;; (strsexp's two-mode parity trick, generalized to a transition table).
;;
;; RUNS. Each char has a render CLASS ('code | 'string | 'comment | 'block |
;; 'charlit) derived from the mode pair around it, so delimiters class with their
;; construct (the opening " is 'string, the ; is 'comment, and a line comment runs
;; through its terminating newline -- a comment ends AT end-of-line, inclusive, the
;; traditional line convention). A run is a maximal span of one class; runs are the
;; natural render/edit pieces. A held `#`
;; classes 'hash -- GLUE: it adheres to the run of whatever follows (`#|`-block,
;; `#\`-charlit, `#"`-string, or plain atom text), so a fragment ending in glue
;; carries a pending flag and the decision defers rightward, exactly like the other
;; edge reconciliations. Adjacent same-class constructs ("a""b") stay two runs: a
;; boundary also fires when the gap's MODE is outside the class's construct.

(require racket/match
         "../rope-core.rkt"        ; make-summary
         "sexp-summary.rkt")       ; sexp-leaf / sexp+ (bare, dispatch-free), sand-spines

(provide lisp-smr
         lisp-state lisp-in-string? lisp-in-comment?
         lisp-spines
         class-sides)

;; scratch-enabling exports (added in summarised-rope2): the arm plumbing the
;; segmenting-split experiment reads. Not part of the file's public surface.
(module+ internal
  (provide (struct-out arm) aref mode->i boundary?))

;; ---------- the mode machine ----------
;; One char in one mode -> (values mode* emitted). The emissions keep the transformed
;; text code-equivalent: a string or char literal reads as the placeholder atom `~`,
;; flanked by spaces so it never fuses with a neighbouring atom; comment interiors
;; vanish (block comments as one space, so atoms across them stay separate).
;;
;; A `#` in code is HELD ('hash) -- its emission is deferred one char: `#\` opens a
;; char literal, `#|` a block comment, anything else re-dispatches as code with the
;; `#` re-emitted in front (so atoms like a#b pass through). Deferral is still
;; deterministic per (mode, char), so the exact concatenation property below holds.
;;
;; The ceiling: block comments do NOT nest (Racket and CL nest theirs) -- nesting
;; depth is unbounded state a finite mode table cannot carry; `#;` datum comments
;; are structural, not lexical (`#` atom + line comment here, as before); a char
;; literal consumes exactly ONE char, so a multi-char name reads as the placeholder
;; plus a trailing atom (`#\space` -> `~ pace` -- inert either way, and `#\"` `#\;`
;; `#\(` are read right, which is what matters); |piped symbols| are ordinary atoms.
(define (step m c)
  (case m
    [(code)       (cond [(char=? c #\") (values 'string " ~")]
                        [(char=? c #\;) (values 'comment " ")]
                        [(char=? c #\#) (values 'hash "")]
                        [else           (values 'code (string c))])]
    [(string)     (cond [(char=? c #\\) (values 'escape "")]
                        [(char=? c #\") (values 'code " ")]
                        [else           (values 'string "")])]
    [(escape)     (values 'string "")]
    [(comment)    (if (char=? c #\newline) (values 'code "\n") (values 'comment ""))]
    [(hash)       (cond [(char=? c #\\) (values 'charlit "")]
                        [(char=? c #\|) (values 'block " ")]
                        [else (let-values ([(m* e) (step 'code c)])
                                (values m* (string-append "#" e)))])]
    [(charlit)    (values 'code " ~ ")]
    [(block)      (if (char=? c #\|) (values 'block-pipe "") (values 'block ""))]
    [(block-pipe) (cond [(char=? c #\#) (values 'code "")]
                        [(char=? c #\|) (values 'block-pipe "")]
                        [else           (values 'block "")])]))

(define modes '(code string escape comment hash charlit block block-pipe))
;; the summary value is a VECTOR of 8 arms, indexed by ENTRY mode in this order
;; (was a #hasheq(mode -> arm)); mode->i is the fixed index of a threaded exit mode.
(define (mode->i m)
  (case m
    [(code) 0] [(string) 1] [(escape) 2] [(comment) 3]
    [(hash) 4] [(charlit) 5] [(block) 6] [(block-pipe) 7]))

;; ---------- run classes ----------
;; The class of a char, from the modes flanking it (before = consumed-in, after =
;; left behind). Delimiters class with their construct; 'hash is the glue class.
(define (class before after)
  (case before
    [(string escape)    'string]                  ; interior, escapes, the closing "
    [(block block-pipe) 'block]                   ; interior and the closing |#
    [(charlit)          'charlit]                 ; the payload char
    [(comment)          'comment]                 ; incl. the terminating \n: a line
                                                  ; comment runs to END of line (traditional)
    [(hash)             (case after
                          [(block)   'block]      ; the | of #|
                          [(charlit) 'charlit]    ; the \ of #\
                          [else (class 'code after)])]           ; re-dispatched
    [else               (case after
                          [(string)  'string]     ; the opening "
                          [(comment) 'comment]    ; the ;
                          [(hash)    'hash]       ; a held # -- glue, defers rightward
                          [else      'code])]))

;; is mode m interior to a construct of class cl? (the adjacency test: two same-class
;; constructs meeting at a gap whose mode is OUTSIDE the construct are separate runs)
(define (inside? cl m)
  (case cl
    [(string)  (and (memq m '(string escape)) #t)]
    [(comment) (eq? m 'comment)]
    [(block)   (and (memq m '(block block-pipe)) #t)]
    [(charlit) (eq? m 'charlit)]
    [else      (eq? m 'code)]))

;; boundary between the run ending in class prev and a following run of class cl,
;; whose deciding gap sits in mode gm ('code when glue adheres -- # only holds from
;; code). #f prev (nothing resolved yet) never bounds.
(define (boundary? prev cl gm)
  (and prev (or (not (eq? prev cl)) (not (inside? cl gm))) #t))

;; ---------- one walk: transform text + run fields ----------
;; Char-wise deterministic, so measure(s1 ++ s2, m) concatenates exactly -- the
;; homomorphism law reduces to sexp-smr's own, and the run fields reconcile at the
;; seam by the same boundary? rule the walk uses internally.
(define (measure s m)
  (define out (open-output-string))
  (define-values (exit n first tailc pend)
    (for/fold ([cm m] [n 0] [first #f] [tailc #f] [pend #f])
              ([c (in-string s)])
      (define-values (cm* e) (step cm c))
      (write-string e out)
      (define cl (class cm cm*))
      (if (eq? cl 'hash)
          (values cm* n first tailc #t)                       ; glue: decision defers
          (values cm*
                  (+ n (if (boundary? tailc cl (if pend 'code cm)) 1 0))
                  (or first cl)
                  cl
                  #f))))
  (values exit (get-output-string out) n first tailc pend))

;; ---------- the summary value ----------
;; Per entry mode, an arm:
;;   exit   mode after the fragment
;;   n      run boundaries strictly inside (under this entry)
;;   first  class this fragment presents leftward ('hash = all glue; #f = empty)
;;   tailc  class of the last RESOLVED run (#f = none)
;;   pend   #t when trailing glue awaits the next fragment's class
;;   val    a PROMISE of the sexp measure of the code-equivalent text under this entry
;; val is lazy (delay/force): combine composes promises instead of running sexp+, so an
;; arm whose val is never read never pays for it -- navigation reads only the code arm and
;; one exit arm, leaving the other ~6 unforced. gen:equal+hash forces val ONLY when two
;; arms are compared, keeping the equal?-based tests (associativity sweep, law battery) green.
(struct arm (exit n first tailc pend val)
  #:methods gen:equal+hash
  [(define (equal-proc a b re)
     (and (re (arm-exit a) (arm-exit b))   (re (arm-n a) (arm-n b))
          (re (arm-first a) (arm-first b)) (re (arm-tailc a) (arm-tailc b))
          (re (arm-pend a) (arm-pend b))
          (re (force (arm-val a)) (force (arm-val b)))))
   (define (hash-proc  a rh) (rh (vector (arm-exit a) (arm-n a) (arm-first a)
                                         (arm-tailc a) (arm-pend a) (force (arm-val a)))))
   (define (hash2-proc a rh) (rh (arm-exit a)))])

;; The summary value is a VECTOR of 8 arm-PROMISES, wrapped in `lval` so equal? forces them.
;; Leaf laziness: an arm's `measure` walk (the per-mode lexer pass, the dominant leaf cost)
;; is deferred until the arm is read, so building a rope allocates promise-vectors and runs
;; NO measure/sexp; navigation forces only the arms on its read path (the code arm, plus one
;; exit arm per cut). aref forces the arm at an index; equal? / hash force all 8 (tests only).
(struct lval (v)          ; v : (vectorof (promise/c arm))
  #:methods gen:equal+hash
  [(define (equal-proc a b re)
     (for/and ([pa (in-vector (lval-v a))] [pb (in-vector (lval-v b))]) (re (force pa) (force pb))))
   (define (hash-proc  a rh) (rh (for/list ([p (in-vector (lval-v a))]) (force p))))
   (define (hash2-proc a rh) (rh 8))])
(define (aref V i) (force (vector-ref (lval-v V) i)))   ; the forced arm at index i

(define (lisp-leaf s)
  (lval (for/vector #:length 8 ([m (in-list modes)])
          (delay (let-values ([(exit text n first tailc pend) (measure s m)])
                   (arm exit n (or first (and pend 'hash)) tailc pend (delay (sexp-leaf text))))))))

;; combine: per entry mode, thread the left's exit into the right's entry -- function
;; composition on modes, sexp's own combine on the values ('malformed absorbs, #f is
;; the unit, both inherited). The run fields reconcile at the seam: the right's
;; presented class against the left's resolved tail, with the left's pending glue
;; adhering to it (gap mode 'code); a right side that is all glue keeps the left
;; pending; empty sides vanish.
(define (lisp+ A B)
  (lval (for/vector #:length 8 ([i (in-range 8)])
          (delay
            (let ()
              (match-define (arm m1 na fa ta pa va) (aref A i))
              (match-define (arm m2 nb fb tb pb vb) (aref B (mode->i m1)))
              (define s (if (and fa fb (not (eq? fb 'hash))
                                 (boundary? ta fb (if pa 'code m1)))
                            1 0))
              (define f (cond [(not fa)       fb]
                              [(eq? fa 'hash) (or fb 'hash)]     ; leading glue adheres into B
                              [else           fa]))
              (define-values (tc pd)
                (cond [(not fb)       (values ta pa)]            ; B empty
                      [(eq? fb 'hash) (values ta #t)]            ; B all glue: still pending
                      [else           (values tb pb)]))
              (arm m2 (+ na nb s) f tc pd (delay (sexp+ (force va) (force vb)))))))))

(define lisp-smr (make-summary lisp-leaf lisp+))

;; ---------- cut reads ----------
;; The document starts in code, so a cut's entry mode = the all-left value's exit
;; from 'code.
(define (lisp-state L) (arm-exit (aref L 0)))
(define (lisp-in-string?  L) (and (memq (lisp-state L) '(string escape)) #t))
(define (lisp-in-comment? L) (and (memq (lisp-state L) '(comment block block-pipe)) #t))

;; spines at a cut: L's code-entry sexp value against R's value under L's exit mode.
(define (lisp-spines L R)
  (match-define (arm m _ _ _ _ vL) (aref L 0))
  (sand-spines (force vL) (force (arm-val (aref R (mode->i m))))))

;; class-sides: read a cut as the pair of run classes touching it -- sand-spines'
;; shape for the lexical layer. Left: the class ending at the cut ('hash = trailing
;; glue, deferring rightward). Right: the class beginning at it, mode-selected.
(define (class-sides L R)
  (match-define (arm m _ _ tc pd _) (aref L 0))
  (values (if pd 'hash tc)
          (arm-first (aref R (mode->i m)))))

;; ========== EXPERIMENTAL: the runs guide* -- multisect* into render pieces =====
;; Provisional, opt-in: (require (submod "lisp-summary.rkt" experimental)). Splits
;; a lisp-smr rope at every run boundary: strings (both quotes), comments (with
;; their ;), block comments and char literals (with their # glue) each come out as
;; whole pieces; code runs stay maximal. Pruned by the cached n, so cost tracks the
;; number of runs, not the document.
;; Self-contained -- delete this submodule to retract.
(module+ experimental
  (require (submod "../rope-core.rkt" experimental))
  (provide lisp-runs-guide*)

  (define lisp-runs-guide*
    (make-guide* lisp-smr
      (lambda (bs fsl fsr as)
        (define l (lisp-smr fsl))
        (define r (lisp-smr fsr))
        (define m0 (arm-exit (aref bs 0)))
        (match-define (arm m1 nl _ tl pl _) (aref l (mode->i m0)))
        (match-define (arm m2 nr _ tr pr _) (aref r (mode->i m1)))
        ;; the class presented after a point: the after-context's first, itself
        ;; 'hash only when glue runs to the document end ( #f = nothing follows)
        (define (as-first m) (arm-first (aref as (mode->i m))))
        (define (resolve f m) (if (or (not f) (eq? f 'hash)) (or (as-first m) 'hash) f))
        (define rl (resolve (arm-first (aref r (mode->i m1))) m2))   ; class right of the seam
        (define rr (or (as-first m2) 'hash))                   ; class right of the focus
        (values (or (> nl 0) (and pl (boundary? tl rl 'code)))    ; a cut inside fsl:
                                                                  ;   counted, or its pending
                                                                  ;   glue resolves to a new run
                (and (not pl) (boundary? tl rl m1))               ; at the seam (never inside
                                                                  ;   a glue group)
                (or (> nr 0) (and pr (boundary? tr rr 'code)))))))

  (module+ test
    (require rackunit "../rope-core.rkt")
    (define build (make-rope lisp-smr))
    (define (pieces s) (map (lambda (p) (format "~a" p)) ((multisect* lisp-runs-guide*) (build s))))

    (check-equal? (pieces "(a \"x y\" b)") '("(a " "\"x y\"" " b)"))   ; both quotes inside
    (check-equal? (pieces "\"a\"\"b\"")    '("\"a\"" "\"b\""))         ; adjacent strings split
    (check-equal? (pieces "x ; c\ny")      '("x " "; c\n" "y"))        ; comment ends AT eol, incl.
    (check-equal? (pieces "a#|x|#b")       '("a" "#|x|#" "b"))         ; glue: # travels with |
    (check-equal? (pieces "(f #\\( x)")    '("(f " "#\\(" " x)"))      ; charlit whole
    (check-equal? (pieces "#\\a#\\b")      '("#\\a" "#\\b"))           ; adjacent charlits split
    (check-equal? (pieces "\"a\"#\"b\"")   '("\"a\"" "#\"b\""))        ; #"..." adheres, splits
    (check-equal? (pieces "a#b c")         '("a#b c"))                 ; atom # never splits
    (check-equal? (pieces "abc")           '("abc"))                   ; no boundaries: one piece
    (check-equal? (pieces "(a ; t (\n b)") '("(a " "; t (\n" " b)"))
    (check-equal? (pieces ";a\n;b")        '(";a\n" ";b"))))           ; adjacent comments split

;; ============================================================================
(module+ test
  (require rackunit)
  (define (code-arm s) (aref (lisp-smr s) 0))
  (define (code-val s) (force (arm-val (code-arm s))))   ; the sexp value, entered in code

  ;; --- the gate: brackets and quotes inside comments / strings / literals are inert ---
  (check-equal? (code-val "(a ; junk ( [ \"\n b)") (sexp-smr "(a  \n b)"))   ; comment interior vanishes
  (check-equal? (code-val "(a \"(\" b)")           (sexp-smr "(a  ~  b)"))   ; string = the ~ atom
  (check-equal? (code-val "(\"))((\")")            (sexp-smr "( ~ )"))       ; parens-full string
  (check-equal? (code-val "(a \"x\\\" y\" b)")     (sexp-smr "(a ~ b)"))     ; escaped quote stays interior
  (check-equal? (code-val "(a ;(\n)")              (sexp-smr "(a  \n)"))     ; unbalanced ( in comment
  (check-equal? (code-val "a\"x\"b")               (sexp-smr "a ~ b"))       ; abutting string: no atom fusion
  (check-equal? (code-val "(a #| x ( \" |# b)")    (sexp-smr "(a b)"))       ; block comment interior vanishes
  (check-equal? (code-val "a#|x|#b")               (sexp-smr "a b"))         ; ...but still separates atoms
  (check-equal? (code-val "(f #\\( x)")            (sexp-smr "(f ~ x)"))     ; #\( opens no level
  (check-equal? (code-val "(f #\\\" x)")           (sexp-smr "(f ~ x)"))     ; #\" opens no string
  (check-equal? (code-val "(f #\\; x)")            (sexp-smr "(f ~ x)"))     ; #\; opens no comment
  (check-equal? (code-val "(a#b c)")               (sexp-smr "(a#b c)"))     ; # inside an atom passes through
  (check-equal? (code-val "(a #\\space b)")        (sexp-smr "(a ~ pace b)")); documented quirk: one char consumed

  ;; 'malformed still absorbs -- but only for brackets the gate lets through
  (check-true  (malformed? (code-val "(a]")))
  (check-false (malformed? (code-val "(a ; ]\n)")))
  (check-false (malformed? (code-val "(a \"]\")")))
  (check-false (malformed? (code-val "(a #| ] |# )")))
  (check-false (malformed? (code-val "(a #\\] )")))

  ;; --- state at a cut ---
  (check-eq? (lisp-state (lisp-smr "(a "))         'code)
  (check-eq? (lisp-state (lisp-smr "(a \""))       'string)
  (check-eq? (lisp-state (lisp-smr "(a \"x\\"))    'escape)
  (check-eq? (lisp-state (lisp-smr "(a \"x\\\""))  'string)     ; the escaped quote did not close
  (check-eq? (lisp-state (lisp-smr "(a \"x\" "))   'code)
  (check-eq? (lisp-state (lisp-smr "(a ; c"))      'comment)
  (check-eq? (lisp-state (lisp-smr "(a ; c\n"))    'code)
  (check-eq? (lisp-state (lisp-smr "(a ; \"x"))    'comment)    ; a quote in a comment opens nothing
  (check-eq? (lisp-state (lisp-smr "(a #"))        'hash)
  (check-eq? (lisp-state (lisp-smr "(a #\\"))      'charlit)
  (check-eq? (lisp-state (lisp-smr "(a #| x"))     'block)
  (check-eq? (lisp-state (lisp-smr "(a #| x |"))   'block-pipe)
  (check-eq? (lisp-state (lisp-smr "(a #| x |# ")) 'code)
  (check-eq? (lisp-state (lisp-smr "#| #| |#"))    'code)       ; ceiling: block comments do NOT nest
  (check-true  (lisp-in-string?  (lisp-smr "\"x\\")))
  (check-false (lisp-in-string?  (lisp-smr "\"x\"")))
  (check-true  (lisp-in-comment? (lisp-smr "; c")))
  (check-false (lisp-in-comment? (lisp-smr "; c\n")))
  (check-true  (lisp-in-comment? (lisp-smr "#| c")))
  (check-true  (lisp-in-comment? (lisp-smr "#| c |")))
  (check-false (lisp-in-comment? (lisp-smr "#| c |#")))

  ;; --- run fields: boundaries counted, edges classed, glue pending ---
  (check-equal? (arm-n (code-arm "(a \"x y\" b)")) 2)      ; code | string | code
  (check-equal? (arm-n (code-arm "\"a\"\"b\""))    1)      ; adjacent strings: two runs
  (check-equal? (arm-n (code-arm "a#b c"))         0)      ; atom # glues into code
  (check-equal? (arm-n (code-arm "a#|x|#b"))       2)      ; code | block | code
  (check-equal? (arm-n (code-arm "#\\a#\\b"))      1)      ; adjacent charlits: two runs
  (check-equal? (arm-n (code-arm "x ; c\ny"))      2)      ; code | comment | code (\n = ws)
  (check-eq?    (arm-first (code-arm "#|x"))       'block) ; leading glue adheres forward
  (check-eq?    (arm-first (code-arm "#"))         'hash)  ; all glue: presented as glue
  (check-true   (arm-pend  (code-arm "a#")))               ; trailing glue: pending
  (check-eq?    (arm-tailc (code-arm "a#"))        'code)  ; ...behind a resolved code run

  ;; --- class-sides: the cut as a pair of run classes ---
  (define (sides l r) (call-with-values (lambda () (class-sides (lisp-smr l) (lisp-smr r))) list))
  (check-equal? (sides "(a "     "\"x\" b)") '(code string))     ; before an opening quote
  (check-equal? (sides "(a \"x"  "y\" b)")   '(string string))   ; inside a string
  (check-equal? (sides "(a \"x\"" " b)")     '(string code))     ; after the closing quote
  (check-equal? (sides "x ; c"   "d\ny")     '(comment comment)) ; inside a comment
  (check-equal? (sides "a#"      "|x|#b")    '(hash block))      ; mid-delimiter: glue defers
  (check-equal? (sides "a#\\"    "q r")      '(charlit charlit)) ; mid char literal

  ;; --- associativity: chunked == whole, cuts landing inside every gated construct ---
  ;; (equal? compares whole arms, so this sweep also pins n / first / tailc / pend)
  (define lisp-corpus
    (list "" "\"" "\"\"" "\"unclosed ("
          "(a \"(\" b)" "(\"))((\")" "(define s \"hi (there)\")"
          "\"esc \\\" quote\"" "(f \"a\\\\\" b)"
          "; comment (unbalanced\n(+ 1 2)" "(a ; trail (\n b)"
          "(a ; \"no string\n b)" "(a \"; no comment\" b)"
          "(a #| x ( \" |# b)" "a#|x|#b" "#|" "|#" "(##)" "(a #| x |"
          "(f #\\( x)" "(f #\\\" x)" "#\\a" "(a #\\space b)" "(a#b c)" "(a #"
          "\"a\"\"b\"" "#\\a#\\b" "\"a\"#\"b\"" "x ; c\ny" "a# b" "##"
          "(define (fact n) ; factorial\n  (if (<= n 1) 1 (* n (fact (- n 1)))))"
          "(let ([msg \"depth = \"]) (display msg))"
          "(a]" "([)]" ") foo (bar"))
  (define (chunked str k)
    (apply lisp-smr (for/list ([i (in-range 0 (string-length str) k)])
                      (substring str i (min (string-length str) (+ i k))))))
  (for* ([str (in-list lisp-corpus)] [k (in-range 1 6)])
    (check-equal? (chunked str k) (lisp-smr str) (format "lisp chunk ~a of ~s" k str)))

  ;; --- the summary-law battery, over text rich in exactly the gated states ---
  (require "summary-laws.rkt" rackcheck)
  (define gen:lisp-text
    (gen:string (gen:one-of (string->list "ab ()[]\";\\\n0#|")) #:max-length 16))
  (check-summary-laws lisp-smr gen:lisp-text #:corpus lisp-corpus)

  ;; --- and on a rope: the cached summary matches the flat string ---
  (define doc "(define (f x) ; twice\n  #| block ( |# (f \"x \\\" y\" #\\( x))")
  (check-equal? (lisp-smr ((make-rope lisp-smr) doc)) (lisp-smr doc)))
