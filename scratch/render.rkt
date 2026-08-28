#lang racket

;; SCRATCH -- render a segmented viewport to the terminal: the display consumer of
;; segment.rkt's output. The tags do all the work: the line factor drives the
;; gutter, the lisp factor drives the colors (strings green, comments dim,
;; charlits magenta, brackets rainbowed by their spine's DEPTH), and the view's
;; 'before/'after blobs collapse to a "lines hidden" marker -- their linecol
;; summary already knows how many. A #:region (cons gl gr) of classic guides
;; paints the 'focus pieces between the cuts on a selection background -- the
;; visual-mode highlight. A #:hl word rides an occur-guide factor in the
;; product and paints its 'occur pieces on an old-gold background (the
;; selection background wins where the two overlap).

(require racket/match
         "segment.rkt"
         "../rope-core.rkt"
         "../summaries/summaries.rkt"
         "../summaries/lisp-summary.rkt"
         "../summaries/occur-summary.rkt")

(provide render-viewport)

;; ---------- ANSI ----------
(define ESC (string (integer->char 27)))
(define (sgr . cs) (format "~a[~am" ESC (string-join (map number->string cs) ";")))
(define (tint text . cs) (string-append (apply sgr cs) text (sgr 0)))

;; the wine-dark palette (256-color): each rainbow hue pulled halfway into its
;; dark neighbour -- claret (red..burgundy), burnt sienna (orange..mahogany),
;; bottle green, marine blue, plum, old gold brackets; bold mulberry keywords;
;; sage-jade strings; rosewood charlits; lapis-slate comments; pearl body text.
(define bracket-tints '(88 130 22 24 96 100))             ; depth -> wine-dark hue, cycling
(define (bracket-tint d) (list-ref bracket-tints (modulo d (length bracket-tints))))

;; ---------- one piece, painted by its lisp tag ----------
;; An opener's spine sits at the parent level, a closer's at its own (the end
;; slot), so depth reads len-1 and len-2 respectively. extra: SGR codes layered
;; under the face -- the selection background rides here.
(define (paint lisp-tag text [extra '()])
  (define (t text . cs) (apply tint text (append extra cs)))
  (match lisp-tag
    [(list 'kw      _) (t text 1 38 5 89)]                ; keyword: bold mulberry
    [(list 'string  _) (t text 38 5 65)]                  ; string: sage-jade
    [(list 'charlit _) (t text 38 5 131)]                 ; charlit: rosewood
    [(list 'comment _) (t text 3 38 5 60)]                ; comment: lapis slate, italic
    [(list 'block   _) (t text 3 38 5 60)]
    [(list 'code spine)
     (match (and (positive? (string-length text)) (string-ref text 0))
       [(or #\( #\[ #\{) (t text 38 5 (bracket-tint (sub1 (length spine))))]
       [(or #\) #\] #\}) (t text 38 5 (bracket-tint (- (length spine) 2)))]
       [_ (t text 38 5 252)])]))                          ; plain code: pearl

(define (hidden-lines piece side)                          ; lines a hidden blob covers
  (define v (linecol-smr piece))
  (+ (linecol-lines v) (if (eq? side 'after) 1 0)))        ; the after blob ends mid-line

;; ---------- the viewport renderer ----------
;; segment with lines*lisp (* cursor when given) under a line-window; walk the
;; pieces in order, opening a gutter at each line start (product pieces are
;; line-bounded, so a piece's only newline is a final one). The cursor paints as
;; a reverse-video block on the first char of the first 'after piece -- the char
;; the cursor gap sits before; at a line end it blocks a phantom space.
(define kw-table (keyword-table "define" "lambda" "let" "if" "cond"))

(define (render-viewport text n m [cursor #f] #:region [region #f] #:hl [hl #f])
  (define doc   (if (rope? text)                 ; a rope renders as-is (its bundle must
                    text                         ;   carry what the factors read); a
                    ((make-rope                  ;   string is roped here
                      (if hl
                          (bundle char-smr linecol-smr lisp-smr atomhash-smr occur-smr)
                          (bundle char-smr linecol-smr lisp-smr atomhash-smr)))
                     text)))
  (define buf   (rope-algebra doc))
  (define lisp+kw ((keywordize kw-table) lisp-run-guide))   ; kw intrinsic in the tags
  (define side-factor  ; a region (pair of classic guides -> 'focus highlighted),
    (cond              ; a classic guide, a (row . col) grid cursor, or a char offset
          [region              (guides->region buf (car region) (cdr region))]
          [(procedure? cursor) (guide->sides buf cursor)]   ; guide reads the bundle whole
          [(pair? cursor)      (guide->sides linecol-smr (grid-at (car cursor) (cdr cursor)))]
          [cursor              (cursor-at cursor)]
          [else                #f]))
  (define inner (apply guide-product
                       (append (list line-guide lisp+kw)
                               (if hl (list (occur-guide hl)) '())
                               (if side-factor (list side-factor) '()))))
  (define segs  ((segment ((line-window n m) inner)) doc))
  (define at-start #t)
  (define armed (and cursor (not region)))       ; block the next 'after piece's first char
  (define (emit line ltag side occ piece)
    (when at-start
      (printf "~a" (tint (format "~a | " (~a line #:min-width 3 #:align 'right)) 38 5 59))
      (set! at-start #f))
    (define s (format "~a" piece))
    (define bg (cond [(eq? side 'focus) '(48 5 238)]   ; selection bg wins over the
                     [(eq? occ 'occur)  '(48 5 58)]    ;   hl's old-gold bg
                     [else              '()]))
    (cond
      [(and armed (eq? side 'after))
       (set! armed #f)
       (define c1 (substring s 0 1))
       (if (equal? c1 "\n")
           (printf "~a~a" (tint " " 7) c1)       ; cursor at a line end: phantom block
           (printf "~a" (tint c1 7)))
       (printf "~a" (paint ltag (substring s 1) bg))]
      [else (printf "~a" (paint ltag s bg))])
    (when (regexp-match? #rx"\n$" s) (set! at-start #t)))
  (for ([seg (in-list segs)])
    (match seg
      [(cons (and side (or 'before 'after)) piece)   ; the VIEW's hidden blobs (bare symbols)
       (printf "~a~n" (tint (format "  ~~ | (~a lines hidden)" (hidden-lines piece side)) 3 38 5 59))
       (when (eq? side 'after) (set! armed #f))  ; the cursor lies in hidden text: no block
       (set! at-start #t)]
      [(cons (cons line (cons ltag rest)) piece)   ; rest: the occ and/or side factors,
       (emit line ltag                             ;   told apart by their tag vocabularies
             (findf (lambda (t) (memq t '(before after focus))) rest)
             (findf (lambda (t) (memq t '(occur plain))) rest)
             piece)]))
  (when armed (printf "~a" (tint " " 7)))        ; never blocked in view: the cursor sits at
  (unless at-start (printf "~n")))               ;   the document end, whatever its kind

;; ============================================================================
(module+ main
  (define doc (string-join '("(define (fact n)      ; the classic"
                             "  (if (<= n 1)"
                             "      1"
                             "      (* n (fact (- n 1)))))"
                             ""
                             "(define greeting \"hello"
                             "world\")"
                             ""
                             "(define ch #\\()"
                             "(display greeting)")
                           "\n"))
  (printf "~n-- the whole document --~n")
  (render-viewport doc 0 99)
  (printf "~n-- viewport: lines 3..6 --~n")
  (render-viewport doc 3 6)
  (define cur (+ 4 (caar (regexp-match-positions #rx"greeting" doc))))
  (printf "~n-- cursor mid-atom: gree|ting (offset ~a) --~n" cur)
  (render-viewport doc 0 99 cur)
  (printf "~n-- same cursor, viewport 3..6 --~n")
  (render-viewport doc 3 6 cur)
  (printf "~n-- grid cursor (1 . 5) --~n")
  (render-viewport doc 0 99 '(1 . 5))
  (printf "~n-- grid cursor (2 . 30): line 2 is ~s -> clamps to the line end --~n" "      1")
  (render-viewport doc 0 99 '(2 . 30))
  (printf "~n-- grid cursor (4 . 3): line 4 is empty -> clamps to column 0 --~n")
  (render-viewport doc 0 99 '(4 . 3)))

