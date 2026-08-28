#lang racket

;; SCRATCH -- face auditions: the wine-dark bracket cycle held fixed, the other
;; faces (keyword / string / charlit / comment) swapped through named option
;; sets, one render per set, for side-by-side comparison in the terminal.

(require racket/match
         "segment.rkt"
         "../rope-core.rkt"
         "../summaries/summaries.rkt"
         "../summaries/lisp-summary.rkt")

(define ESC (string (integer->char 27)))
(define (sgr . cs) (format "~a[~am" ESC (string-join (map number->string cs) ";")))
(define (tint text . cs) (string-append (apply sgr cs) text (sgr 0)))

(define bracket-tints '(88 130 22 24 96 100))             ; the keeper: wine-dark cycle
(define (bracket-tint d) (list-ref bracket-tints (modulo d (length bracket-tints))))

;; a face set: kw / string / charlit / comment 256-color codes (kw renders bold,
;; comment italic)
;; set 4 held (olive strings, bronze charlits, bark comments); the keyword hue
;; toured across entirely different families, none used by the bracket cycle
(define option-sets
  '(("teal        (31)"  31  101 137 59)
    ("turquoise   (37)"  37  101 137 59)
    ("viridian    (35)"  35  101 137 59)
    ("gold        (178)" 178 101 137 59)
    ("copper      (166)" 166 101 137 59)
    ("cornflower  (68)"  68  101 137 59)
    ("indigo      (62)"  62  101 137 59)
    ("bone        (230)" 230 101 137 59)))

(define ((mk-paint kw str chl cmt) ltag text)
  (match ltag
    [(list 'kw      _) (tint text 1 38 5 kw)]
    [(list 'string  _) (tint text 38 5 str)]
    [(list 'charlit _) (tint text 38 5 chl)]
    [(list 'comment _) (tint text 3 38 5 cmt)]
    [(list 'block   _) (tint text 3 38 5 cmt)]
    [(list 'code spine)
     (match (and (positive? (string-length text)) (string-ref text 0))
       [(or #\( #\[ #\{) (tint text 38 5 (bracket-tint (sub1 (length spine))))]
       [(or #\) #\] #\}) (tint text 38 5 (bracket-tint (- (length spine) 2)))]
       [_ (tint text 38 5 252)])]))

(define doc "(define (greet n)          ; salute\n  (if n (show \"hi there\" #\\!) 'quiet))")

(define buf      (bundle char-smr lisp-smr tokhash-smr))
(define kw-table (keyword-table "define" "lambda" "let" "if" "cond"))
(define segs     ((keywords kw-table) ((segment lisp-run-guide) ((make-rope buf) doc))))

(for ([opt (in-list option-sets)])
  (match-define (list label kw str chl cmt) opt)
  (define paint (mk-paint kw str chl cmt))
  (printf "~n~a~n" (tint (format "-- ~a --" label) 38 5 245))
  (for ([seg (in-list segs)])
    (printf "~a" (paint (car seg) (format "~a" (cdr seg)))))
  (printf "~n"))
