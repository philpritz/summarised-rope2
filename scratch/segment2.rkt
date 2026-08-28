#lang racket

;; SCRATCH -- segment2: the guide algebra over CALLABLE STRUCTS (draft; the
;; design discussion is this session's). Supersession candidate for parts of
;; segment.rkt (line-window, segment/memo, covering-key, guide/s) -- undecided;
;; segment.rkt stands untouched and serves as the referee in the demos.
;;
;; ALL GUIDES ARE FRAMED. A guide is one struct:
;;
;;   (struct guide (smr bs as fn))
;;
;; smr is the guide's LENS: it seeds the empty contexts, accumulates them on
;; descent, and projects every value the fn sees -- fns are written against
;; their own summary domain, with no manual normalization. bs/as are the
;; accumulated contexts, live only while a walk (or a deliberate frame) puts
;; them there; make-guide always starts them empty, so context enters a guide
;; ONLY through the walk's folds or through frame -- there is no bare-guide /
;; framed-guide distinction to get wrong.
;;
;; Guides are CALLABLE, one arity:
;;   (g fs)          contexts from the fields -- the ONLY door. A caller with
;;                   context in hand frames first: ((frame g b a) fs). A
;;                   composite calls its factors the same way, each factor
;;                   re-projecting through its own lens.
;;
;; (struct viewport (n m inner)) is the view -- rows n..m over an inner guide,
;; interrogable by the memo key. Callable the same way; its contexts live in
;; INNER's fields (the folds rebuild it around the extended inner). Depth-one
;; by assumption: a viewport wraps a plain guide.
;;
;; LENS REQUIREMENTS (a consequence of bundle projection being atomic-or-
;; identity: a bundle value projects its components, or passes through its
;; own bundle -- it does NOT project sub-bundles): single-summary guides
;; (line-guide, cursor-at, occur-guide) carry their atomic lens and run over
;; any bundle containing it; multi-summary guides (lisp-run-guide, the
;; classic lifts, keywordize's wrap) take the WORKING BUNDLE as their lens.
;; For memoized use the top guide's lens must carry hash-smr (context
;; fingerprints) and linecol-smr (the viewport's spans); a narrow-lens guide
;; used alone keys its contexts by their raw summary values instead -- sound
;; exactly because such a guide reads nothing else.
;;
;; The old classifier bodies run VERBATIM inside the new structs (their
;; internal projections become pass-throughs on already-projected values), so
;; lisp-run-guide, occur-guide, keywordize, guide->sides and guides->region
;; wrap segment.rkt's functions rather than duplicate them -- and conversely
;; as-old turns a struct back into an old-style (bs fs as) classifier (frame,
;; then the field call), so segment.rkt's OLD segment still referees the demos.

(require racket/match racket/list
         (except-in "../rope-core.rkt" frame)    ; rope-core's frame serves classic
         (submod "../rope-core.rkt" internal)    ;   cut guides; ours shadows it here
         "../summaries/summaries.rkt"
         "../summaries/lisp-summary.rkt"
         "../summaries/occur-summary.rkt"
         "../toolbox/memoize.rkt"
         (prefix-in old: "segment.rkt"))

(provide (struct-out guide) (struct-out viewport)
         make-guide frame as-old guide-left guide-right line-span
         segment viewport-key
         line-guide lisp-run-guide occur-guide cursor-at
         guide-product keywordize guide->sides guides->region)

;; ---------- rope plumbing (after segment.rkt's; private there) ----------
;; The rope fn (a make-rope factory) is taken EXPLICITLY throughout -- the
;; walk does no summary work at all (that all lives in the guides), so its
;; only algebra need is this factory, and nothing derives via rope-algebra.
(define (split rope t)
  (if (branch? t)
      (values (branch-left t) (branch-right t))
      (let* ([s (leaf-text t)] [mid (quotient (string-length s) 2)])
        (values (rope (substring s 0 mid)) (rope (substring s mid))))))
(define (indivisible? t) (and (leaf? t) (<= (string-length (leaf-text t)) 1)))

;; ---------- the guide struct ----------
(struct guide (smr bs as fn)
  #:property prop:procedure
  (lambda (self fs)                              ; the single arity: contexts from
    (define s (guide-smr self))                  ;   the fields, all lens-projected
    ((guide-fn self) (s (guide-bs self)) (s fs) (s (guide-as self)))))

(define (make-guide smr fn) (guide smr (smr "") (smr "") fn))

;; ---------- the viewport ----------
(define (line-span bs fs)                        ; lo/hi rows of a focus
  (define b (linecol-smr bs))
  (define f (linecol-smr fs))
  (define lo (linecol-lines b))
  (define hi (+ lo (linecol-lines f)
                (if (and (> (linecol-lines f) 0) (zero? (linecol-cols f))) -1 0)))
  (values lo hi))

(define (viewport-judge v bs fs delegate)
  (define-values (lo hi) (line-span bs fs))
  (cond [(< hi (viewport-n v))                            'before]
        [(> lo (viewport-m v))                            'after]
        [(and (>= lo (viewport-n v)) (<= hi (viewport-m v))) (delegate)]
        [else                                             #f]))

(struct viewport (n m inner)
  #:property prop:procedure
  (lambda (self fs)
    (define g (viewport-inner self))
    (viewport-judge self (guide-bs g) fs (lambda () (g fs)))))

;; ---------- context: the four one-sided folds ----------
;; frame folds OUTER context on (embed the guide in b..a -- the deliberate
;; fragment-in-context entrance); guide-left/right fold INNER context on (the
;; walk's descent steps: the sibling just crossed sits between the old
;; context and the focus). Associativity is what lets them compose freely.
(define (frame g b a)
  (if (viewport? g)
      (viewport (viewport-n g) (viewport-m g) (frame (viewport-inner g) b a))
      (let ([s (guide-smr g)])
        (struct-copy guide g [bs (s b (guide-bs g))] [as (s (guide-as g) a)]))))

(define (guide-left g l)                         ; descend right: l joins bs
  (if (viewport? g)
      (viewport (viewport-n g) (viewport-m g) (guide-left (viewport-inner g) l))
      (struct-copy guide g [bs ((guide-smr g) (guide-bs g) l)])))

(define (guide-right g r)                        ; descend left: r joins as
  (if (viewport? g)
      (viewport (viewport-n g) (viewport-m g) (guide-right (viewport-inner g) r))
      (struct-copy guide g [as ((guide-smr g) r (guide-as g))])))

(define (as-old g)                               ; a struct as an old-style (bs fs as)
  (lambda (bs fs as) ((frame g bs as) fs)))      ;   classifier: frame, then the call

;; ---------- the memo key ----------
;; key : t g -> key | #f. The focus enters by its INTRINSIC fields (rope-hash,
;; rope-length -- nothing assumed of the bundle); a context enters by its hash
;; component when its lens carried hash-smr, else by its RAW value --
;; self-keying: a narrow guide reads nothing its context value doesn't carry,
;; so equal values imply equal answers. The guide slot is (smr . fn), the
;; parts STABLE under the walk's struct-copies -- and for a focus covered by
;; a viewport it is the INNER's, so every covering viewport (and the bare
;; inner itself) shares one entry set. #f -- no entry: straddling /
;; out-of-view / tiny foci.
(define (ctx-key v)
  (if (summary-part? v)
      (or (part->summary v hash-smr #f) v)     ; bundle: its fingerprint, if any
      v))                                      ; narrow value: itself
(define ((viewport-key [min-chars 4]) t g)
  (and (>= (rope-length t) min-chars)
       (let ([in (if (viewport? g)
                     (let ([in (viewport-inner g)])
                       (define-values (lo hi) (line-span (guide-bs in) t))
                       (and (>= lo (viewport-n g)) (<= hi (viewport-m g)) in))
                     g)])
         (and in
              (list (rope-hash t)
                    (ctx-key (guide-bs in)) (ctx-key (guide-as in))
                    (cons (guide-smr in) (guide-fn in)))))))

;; ---------- the walk, its body worn by the cache ----------
(define (splice rope a b)                        ; weld the one seam
  (cond [(null? a) b]
        [(null? b) a]
        [else
         (match-define (cons tg p)  (last a))
         (match-define (cons tg2 q) (car b))
         (if (equal? tg tg2)
             (append (drop-right a 1) (cons (cons tg (rope p q)) (cdr b)))
             (append a b))]))

(define (segment rope #:key [key (viewport-key)] #:cap [cap 4096])
  (define step
    (memoize
     (lambda (t g)                               ; -> t's seglist, tail-free
       (cond
         [(zero? (rope-leaves t)) '()]
         [(g t) => (lambda (tag) (list (cons tag t)))]     ; the field call judges
         [(indivisible? t)
          (error 'segment "guide refused an indivisible piece: ~v" (leaf-text t))]
         [else
          (define-values (l r) (split rope t))
          (splice rope
                  (step l (guide-right g r))     ; recursion through STEP, the
                  (step r (guide-left g l)))]))  ;   memoized identity
     #:key key
     #:cap cap))
  step)                                          ; the segmenter IS the memo:
                                                 ;   memo-on!/off!/size! reach it
;; ---------- the guides ----------
;; new-style fns (projected values, zero boilerplate):
(define line-guide
  (make-guide linecol-smr
    (lambda (b f a)                              ; b f a: linecol values
      (and (or (zero? (linecol-lines f))
               (and (= (linecol-lines f) 1) (zero? (linecol-cols f))))
           (linecol-lines b)))))

(define (cursor-at p)
  (make-guide char-smr
    (lambda (b f a)                              ; b f a: char counts
      (cond [(<= (+ b f) p) 'before]
            [(>= b p)       'after]
            [else           #f]))))

;; the old bodies, wrapped (their internal projections pass through):
(define (lisp-run-guide B)  (make-guide B old:lisp-run-guide))   ; B: char+lisp at least
(define (occur-guide W)     (make-guide occur-smr (old:occur-guide W)))
(define (guide->sides B g)  (make-guide B (old:guide->sides B g)))
(define (guides->region B gl gr) (make-guide B (old:guides->region B gl gr)))
(define ((keywordize table) g)                   ; g's lens must carry atomhash
  (make-guide (guide-smr g) ((old:keywordize table) (as-old g))))

(define (guide-product smr . gs)                 ; factors re-lens: framed, then called
  (make-guide smr
    (lambda (bs fs as)
      (let loop ([gs gs] [acc '()])
        (cond [(null? gs) (reverse acc)]
              [((frame (car gs) bs as) fs) => (lambda (tag) (loop (cdr gs) (cons tag acc)))]
              [else #f])))))

;; ============================================================================
(module+ main
  (define B    (bundle char-smr linecol-smr lisp-smr hash-smr))
  (define R    (make-rope B))
  (define G    (guide-product B line-guide (lisp-run-guide B)))
  (define text (apply string-append
                      (for/list ([t '((a b c) (d e f) (g h i) (j k l)
                                      (m n o) (p q r) (s t u) (v w x))])
                        (format "(def ~a\n  (~a ~a))\n" (first t) (second t) (third t)))))
  (define doc  (R text))

  (define G/old (as-old G))                              ; the OLD walk referees the
  (define (view segs) (map (lambda (s) (cons (car s) (format "~a" (cdr s)))) segs))
  (define (referee g) (view ((old:segment g) doc)))      ;   NEW structs via as-old
  ;; a segmenter instance with counters riding the key
  (define (make-seg)
    (define stats (make-hash))
    (define (bump! k) (hash-update! stats k add1 0))
    (define k0 (viewport-key))
    (define seg (segment R #:key (lambda (t g)
                                 (define k (k0 t g))
                                 (bump! (if k 'keyed 'unkeyed))
                                 k)))
    (values seg stats))
  (define ((frame! seg stats [d #f]) label g ref)
    (define doc* (or d doc))
    (define before-k (hash-ref stats 'keyed 0))
    (define before-u (hash-ref stats 'unkeyed 0))
    (define before-e (memo-size seg))
    (define segs (seg doc* g))
    (define misses (- (memo-size seg) before-e))
    (define keyed  (- (hash-ref stats 'keyed 0) before-k))
    (define unkeyed (- (hash-ref stats 'unkeyed 0) before-u))
    (printf "  ~a  pieces ~a   hits ~a  misses ~a  uncached ~a   entries ~a   agrees ~a~n"
            (~a label #:min-width 24) (~a (length segs) #:min-width 2)
            (~a (max 0 (- keyed misses)) #:min-width 3) (~a misses #:min-width 3)
            (~a unkeyed #:min-width 3) (~a (memo-size seg) #:min-width 3)
            (if (equal? (view segs) ref) 'yes 'NO)))

  (printf "========== viewports larger than the rope share ONE entry set ==========~n")
  (let-values ([(seg stats) (make-seg)])
    (define f! (frame! seg stats))
    (f! "viewport 0..99"    (viewport 0 99 G)   (referee ((old:line-window 0 99) G/old)))
    (f! "viewport 0..999"   (viewport 0 999 G)  (referee ((old:line-window 0 999) G/old)))
    (f! "viewport -5..500"  (viewport -5 500 G) (referee ((old:line-window -5 500) G/old)))
    (f! "no viewport at all" G                  (referee G/old)))

  (printf "~n========== a viewport scrolled down and back up (cold cache) ==========~n")
  (let-values ([(seg stats) (make-seg)])
    (define f! (frame! seg stats))
    (define (fr label n m) (f! label (viewport n m G) (referee ((old:line-window n m) G/old))))
    (fr "rows 0..7   first paint" 0 7)
    (fr "rows 4..11  scroll down" 4 11)
    (fr "rows 8..15  scroll down" 8 15)
    (fr "rows 4..11  scroll UP"   4 11)
    (fr "rows 0..7   scroll UP"   0 7)
    (fr "rows 2..9   misaligned"  2 9)
    (printf "~n---------- the switch, same instance ----------~n")
    (memo-off! seg)
    (fr "rows 0..7   cache OFF"   0 7)
    (memo-on! seg)
    (fr "rows 0..7   cache ON"    0 7))

  (printf "~n========== scrolling down by ONE row (64 lines, cold cache) ==========~n")
  (let-values ([(seg stats) (make-seg)])
    (define doc2 (R (apply string-append
                           (for/list ([i 32]) (format "(def f~a\n  (g~a x))\n" i i)))))
    (define f! (frame! seg stats doc2))
    (for ([n (in-range 0 8)])
      (f! (format "rows ~a..~a" (~a n #:min-width 2) (+ n 7))
          (viewport n (+ n 7) G)
          (view ((old:segment ((old:line-window n (+ n 7)) G/old)) doc2)))))

  (printf "~n========== narrow lens alone: line-guide, contexts self-key ==========~n")
  (let-values ([(seg stats) (make-seg)])
    (define f! (frame! seg stats))
    (f! "line-guide, paint"   line-guide (referee (as-old line-guide)))
    (f! "line-guide, repaint" line-guide (referee (as-old line-guide))))

  (printf "~n========== frame: a fragment judged in context ==========~n")
  (let* ([frag (R (substring text 32 48))]             ; the whole 3rd form -- its
         [b (substring text 0 32)]                     ;   edges land on piece
         [a (substring text 48 (string-length text))]  ;   boundaries of the doc
         [seg (segment R)]
         [segs (seg frag (frame G b a))])
    (printf "  fragment ~s framed by its true surroundings:~n" (format "~a" frag))
    (for ([s (in-list segs)])
      (printf "    ~a  ~s~n" (~a (car s) #:min-width 20) (format "~a" (cdr s))))
    ;; the same pieces must appear inside the WHOLE document's segmentation
    (define whole ((old:segment G/old) doc))
    (printf "  every framed piece tagged as in the whole doc: ~a~n"
            (if (for/and ([s (in-list segs)])
                  (member (cons (car s) (format "~a" (cdr s))) (view whole)))
                'yes 'NO)))

  (printf "~n========== keywordize + cursor + region over the structs ==========~n")
  (let* ([KB   (bundle char-smr linecol-smr lisp-smr old:atomhash-smr hash-smr)]
         [tbl  (old:keyword-table "def")]
         [G+   (guide-product KB line-guide ((keywordize tbl) (lisp-run-guide KB))
                              (cursor-at 6))]
         [d    ((make-rope KB) "(def a\n  (b c))")]
         [seg  (segment (make-rope KB))]
         [segs (seg d G+)])
    (for ([s (in-list segs)])
      (printf "  ~a  ~s~n" (~a (car s) #:min-width 30) (format "~a" (cdr s))))
    (printf "  agrees with old segment on the same struct: ~a~n"
            (if (equal? (view segs) (view ((old:segment (as-old G+)) d))) 'yes 'NO))))
