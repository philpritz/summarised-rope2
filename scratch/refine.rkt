#lang racket

;; SCRATCH -- refine: the second stage of a two-stage segmentation, and a
;; bench of it against the one-stage recut. Draft; design discussion is this
;; session's.
;;
;;   stage 1  (MEMOIZED)  viewport x SYNTAX -- changes when the text or the
;;                        window changes. segment2's memo keys strip a
;;                        covering viewport, so scrolling reuses subtree
;;                        seglists.
;;   stage 2  (REFINE)    the CURSOR -- changes every keystroke. List-local,
;;                        and deliberately NOT memoized: a cursor-tagged
;;                        entry in the slow cache would age it document-wide.
;;
;; The factorization is exact -- the cut counterpart to restyle.rkt's
;; coagulation equation (which joins):
;;
;;     segment (guide-product smr g1 g2)  =  refine g2 . segment g1
;;
;; for g2 HEREDITARY (tags a focus => tags every sub-focus the same). Without
;; it a piece stage 1 welded from two halves could be tagged whole by g2
;; where the fused walk would have descended into it. The side / block /
;; region factors are position thresholds, so they qualify. The demo checks
;; the law frame by frame against the one-stage walk.
;;
;; refine recovers each piece's contexts FROM THE LIST -- one scanl and one
;; scanr over the pieces -- rather than carrying a frame inside the piece.
;; Consequences: nothing is stored, welding has no frames to reconcile, and
;; stage 1's memoized values stay context-free (which is what keeps the
;; context-projections and relative-tags directions open). smr is the
;; refiner's lens and the only algebra refine needs beyond the rope fn: with
;; a cursor it is char-smr, so the scans are integer adds over cached reads.
;;
;; refine-at is the cut-set specialization: refine at OFFSETS, no guide and
;; no scans -- piece lengths are intrinsic (rope-length), so locating the
;; cuts is integer arithmetic and only the straddling pieces are touched.

(require racket/match racket/list
         (except-in "../rope-core.rkt" frame)
         "../summaries/summaries.rkt"
         "../summaries/lisp-summary.rkt"
         "../toolbox/memoize.rkt"
         "segment2.rkt"
         (prefix-in old: "segment.rkt"))

(provide scanl scanr splice refine refine-at cursor-block-at no-memo tag= extend)

;; ---------- the seam weld ----------
;; segment2's splice, generalized to #:merge (segment.rkt's partial semigroup
;; on tags). ONE seam is enough, and that is exactly what the associativity
;; obligation -- definedness included -- buys: if a[-2](+)a[-1] is undefined
;; then so is a[-2](+)(a[-1](+)b[0]), so a weld can never cascade leftward.
(define (tag= a b) (and (equal? a b) a))

(define (splice rope merge a b)
  (cond [(null? a) b]
        [(null? b) a]
        [else
         (match-define (cons t1 p) (last a))
         (match-define (cons t2 q) (car b))
         (define m (merge t1 t2))
         (if m
             (append (drop-right a 1) (cons (cons m (rope p q)) (cdr b)))
             (append a b))]))

;; ---------- the contexts, recovered from the list ----------
;; Pieces are ropes, so (smr p) is a cached read -- a projection when smr is
;; narrow. scanr's fold builds its result already in order; scanl's reverses.
(define (scanl smr ps)                           ; exclusive prefixes: each piece's bs
  (for/fold ([v (smr "")] [acc '()] #:result (reverse acc)) ([p (in-list ps)])
    (values (smr v p) (cons v acc))))

(define (scanr smr ps)                           ; exclusive suffixes: each piece's as
  (for/fold ([v (smr "")] [acc '()] #:result acc) ([p (in-list (reverse ps))])
    (values (smr p v) (cons v acc))))

;; ---------- refine ----------
;;   #:tag    parent x child -> tag. The default pairs them, so the law is an
;;            identity on the nose against (guide-product smr g1 g2). extend
;;            is the flattening variant, for a stage 1 that is itself a
;;            product.
;;   #:merge  the seam weld (above).
;;   #:walk   the stage-2 walk. Hoist it: building one per frame allocates a
;;            fresh cache for a memo that must never fire anyway.
(define (extend parent child) (append parent (list child)))
(define (no-memo t g) #f)                        ; the refusing key = a plain walk

(define (refine rope smr g
                #:tag   [tag list]
                #:merge [merge tag=]
                #:walk  [walk (segment rope #:key no-memo)])
  (lambda (segs)
    (define ps (map cdr segs))
    (foldr (lambda (s bs as tail)
             (splice rope merge
                     (for/list ([c (in-list (walk (cdr s) (frame g bs as)))])
                       (cons (tag (car s) (car c)) (cdr c)))
                     tail))
           '() segs (scanl smr ps) (scanr smr ps))))

;; ---------- refine-at: the cut-set specialization ----------
;; cuts: a sorted list of char offsets. Each piece's tag is extended by the
;; index of the interval it lands in, so the cursor's (p, p+1) gives
;; before / block / after by position alone -- no guide, no contexts, no
;; summaries. smr is needed only to split a straddling piece.
(define (refine-at rope smr cuts #:tag [tag list] #:merge [merge tag=])
  (define (push acc tg piece)                    ; weld onto the reversed accumulator
    (match acc
      [(cons (cons t0 p0) rest)
       (define m (merge t0 tg))
       (if m (cons (cons m (rope p0 piece)) rest) (cons (cons tg piece) acc))]
      [_ (cons (cons tg piece) acc)]))
  (lambda (segs)
    (let loop ([segs segs] [pos 0] [i 0] [cs cuts] [acc '()])
      (cond
        [(and (pair? cs) (<= (car cs) pos)) (loop segs pos (add1 i) (cdr cs) acc)]
        [(null? segs) (reverse acc)]
        [else
         (match-define (cons parent piece) (car segs))
         (define len (rope-length piece))
         (cond
           [(and (pair? cs) (< (car cs) (+ pos len)))          ; a cut lands strictly inside
            (define-values (l r) ((multisect smr (old:char-at (- (car cs) pos))) piece))
            (loop (cons (cons parent r) (cdr segs)) (car cs) (add1 i) (cdr cs)
                  (push acc (tag parent i) l))]
           [else
            (loop (cdr segs) (+ pos len) i cs (push acc (tag parent i) piece))])]))))

;; ---------- the cursor as a piece ----------
;; mini-edit2's display cursor in the framed style: the cursor's own char is
;; its own piece ('block), everything else takes a side. Total on single
;; chars, and hereditary -- so the refinement law applies to it.
(define (cursor-block-at p)
  (make-guide char-smr
    (lambda (b f a)
      (cond [(<= (+ b f) p)        'before]
            [(>= b (add1 p))       'after]
            [(and (= b p) (= f 1)) 'block]
            [else                  #f]))))

;; ============================================================================
(module+ main
  (define B   (bundle char-smr linecol-smr lisp-smr old:atomhash-smr hash-smr))
  (define R   (make-rope B))
  (define tbl (old:keyword-table "define" "lambda" "let" "if" "cond"))

  ;; THE syntax guide -- built ONCE. A guide rebuilt per frame is a fresh
  ;; (smr . fn) in the memo key, hence zero hits; persistence is a
  ;; requirement of the design, not an optimization.
  (define G1 ((keywordize tbl) (lisp-run-guide B)))

  (define text
    (apply string-append
           (for/list ([i (in-range 40)])
             (format (string-append "(define (f~a x)        ; helper ~a\n"
                                    "  (if (< x ~a)\n"
                                    "      (g \"s~a\" x)\n"
                                    "      (h ~a (f~a (- x 1)))))\n\n")
                     i i i i i i))))
  (define doc (R text))

  (define row-offsets                            ; line starts, for placing the cursor
    (let loop ([i 0] [acc '(0)])
      (cond [(= i (string-length text)) (list->vector (reverse acc))]
            [(char=? (string-ref text i) #\newline) (loop (add1 i) (cons (add1 i) acc))]
            [else (loop (add1 i) acc)])))
  (define nrows (sub1 (vector-length row-offsets)))
  (define (at r c) (+ (vector-ref row-offsets r) c))
  (define H 20)                                  ; window height, in rows

  (printf "document: ~a chars, ~a rows, ~a-row window~n"
          (string-length text) nrows H)

  (define (view segs) (map (lambda (s) (cons (car s) (format "~a" (cdr s)))) segs))
  (define (insert d p s)                         ; one keystroke: a char into the rope
    (define-values (l r) ((multisect char-smr (old:char-at p)) d))
    (R l s r))
  ;; refine-at names its intervals by index; (p, p+1) makes 0/1/2 the cursor's
  ;; three sides, which is the tag the guide version produces.
  (define (side parent i) (list parent (vector-ref #(before block after) i)))

  ;; ---------- instrumentation ----------
  (define (counting-key)                         ; stage-1 key + a step counter
    (define stats (make-hash))
    (define k0 (viewport-key))
    (values (lambda (t g)
              (define k (k0 t g))
              (hash-update! stats (if k 'keyed 'unkeyed) add1 0)
              k)
            stats))
  (define (counting-no-memo)                     ; a refusing key that counts steps
    (define n (box 0))
    (values (lambda (t g) (set-box! n (add1 (unbox n))) #f) n))

  ;; ---------- the four pipelines ----------
  ;; each returns (values frame-fn report-fn); a frame is (doc n m p), so the
  ;; typing scenario can vary the document. report -> (list s1 s2 entries).
  (define (fresh-two)                            ; stage 1 memoized, then refine
    (define-values (k stats) (counting-key))
    (define-values (k2 steps2) (counting-no-memo))
    (define seg   (segment R #:key k))
    (define plain (segment R #:key k2))          ; the stage-2 walk, hoisted
    (values (lambda (d n m p)
              ((refine R char-smr (cursor-block-at p) #:walk plain)
               (seg d (viewport n m G1))))
            (lambda () (list (+ (hash-ref stats 'keyed 0) (hash-ref stats 'unkeyed 0))
                             (unbox steps2) (memo-size seg)))))

  (define (fresh-cuts)                           ; stage 1 memoized, then refine-at
    (define-values (k stats) (counting-key))
    (define seg (segment R #:key k))
    (values (lambda (d n m p)
              ((refine-at R char-smr (list p (add1 p)) #:tag side)
               (seg d (viewport n m G1))))
            (lambda () (list (+ (hash-ref stats 'keyed 0) (hash-ref stats 'unkeyed 0))
                             0 (memo-size seg)))))

  (define (fresh-one)                            ; one stage, the cursor IN the guide
    (define-values (k stats) (counting-key))
    (define seg (segment R #:key k))
    (values (lambda (d n m p)
              (seg d (guide-product B (viewport n m G1) (cursor-block-at p))))
            (lambda () (list (+ (hash-ref stats 'keyed 0) (hash-ref stats 'unkeyed 0))
                             0 (memo-size seg)))))

  (define (fresh-none)                           ; the same, with no memo at all
    (define-values (k steps) (counting-no-memo))
    (define seg (segment R #:key k))
    (values (lambda (d n m p)
              (seg d (guide-product B (viewport n m G1) (cursor-block-at p))))
            (lambda () (list (unbox steps) 0 0))))

  (define pipelines (list (cons "two-stage: memo + refine"     fresh-two)
                          (cons "two-stage: memo + refine-at"  fresh-cuts)
                          (cons "one-stage: memo, cursor in g" fresh-one)
                          (cons "one-stage: no memo"           fresh-none)))

  (define (time-it thunk)
    (collect-garbage)
    (define t0 (current-inexact-milliseconds))
    (thunk)
    (- (current-inexact-milliseconds) t0))

  (define (run label frames)
    (printf "~n========== ~a  (~a frames) ==========~n" label (length frames))
    ;; the law: every pipeline must agree with the one-stage walk, every frame
    (let-values ([(ref _r) (fresh-one)])
      (for ([entry (in-list pipelines)])
        (define-values (f _) ((cdr entry)))
        (define bad (for/first ([fr (in-list frames)]
                                #:unless (equal? (view (apply f fr)) (view (apply ref fr))))
                      fr))
        (when bad (printf "  !! ~a disagrees at ~a~n" (car entry) (cdr (cdr bad)))))
      (printf "  law: all pipelines agree with the one-stage walk, every frame~n")
      (printf "  pieces/frame ~a~n" (length (apply ref (car frames)))))
    (printf "  ~a ~a ~a ~a ~a~n"
            (~a "pipeline" #:min-width 30) (~a "ms" #:min-width 9)
            (~a "ms/frame" #:min-width 9) (~a "walk steps" #:min-width 14)
            (~a "entries" #:min-width 8))
    (for ([entry (in-list pipelines)])
      (define-values (f report) ((cdr entry)))
      (define ms (time-it (lambda () (for ([fr (in-list frames)]) (void (apply f fr))))))
      (match-define (list s1 s2 entries) (report))
      (printf "  ~a ~a ~a ~a ~a~n"
              (~a (car entry) #:min-width 30)
              (~a (~r ms #:precision 1) #:min-width 9)
              (~a (~r (/ ms (length frames)) #:precision 3) #:min-width 9)
              (~a (if (zero? s2) (format "~a" s1) (format "~a + ~a" s1 s2)) #:min-width 14)
              (~a entries #:min-width 8))))

  ;; ---------- the scenarios ----------
  (define N 60)

  ;; 1. scrolling: the window slides a row at a time, the cursor riding with it
  (run "scrolling, one row at a time"
       (for/list ([n (in-range N)])
         (list doc n (+ n H -1) (at (+ n 2) 4))))

  ;; 2. the keystroke case: the window stands still, the cursor walks
  (run "cursor moving, window fixed"
       (for/list ([k (in-range N)])
         (list doc 0 (sub1 H) (+ (at 0 0) k))))

  ;; 3. both at once
  (run "cursor moving AND scrolling"
       (for/list ([n (in-range N)])
         (list doc n (+ n H -1) (at (+ n 2) (modulo n 12)))))

  ;; 4. repaint: the same frame over and over -- the pure-hit floor
  (run "repaint, identical frame"
       (for/list ([_ (in-range N)]) (list doc 0 (sub1 H) (at 2 4))))

  ;; 5. typing: a char inserted into the window every frame. The text changes,
  ;; so contexts change, so every fingerprint-keyed entry downstream of the
  ;; edit is a miss -- the honest limit of a cache keyed by full contexts,
  ;; and what the context-projections upgrade is for.
  (run "typing, one char per frame"
       (let loop ([i 0] [d doc] [acc '()])
         (define q (at 3 10))
         (if (= i N)
             (reverse acc)
             (let ([d* (insert d q "z")])
               (loop (add1 i) d* (cons (list d* 0 (sub1 H) (add1 q)) acc)))))))

