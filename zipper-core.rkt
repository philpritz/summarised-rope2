#lang racket

;; Zipper: structured navigation + editing over a summarised rope, as a stack machine.
;; A cursor is two guides, start gs + end ge (gap = gs=ge, seg = gs<ge); the surface:
;;   start         smr rope gs ge -> zipper      -- whole rope as focus, cursor installed
;;   zipper-focus/g  the focus optic in the STAGED (g*) protocol (toolbox/stage.rkt),
;;                 the flank summaries on the render bus; driven by stage-get/-set/-update.
;;   zipper-guide/g  (zipper-guide/g i): edge i's guide focal, its cut on the bus.
;;   zipper-guides   the both-edges guide optic: the pair (gs ge) focal, both edges' cuts
;;                 on the bus as parallel lists (Ls Rs) -- zipper-guide/g's list-valued twin.
;;   edge-view     (edge-view i): edge i's cut as (values L R) -- a VIEWER, not an optic
;;   to-root       fold the crumbs back -- the focus becomes the whole document
;; All optics are STAGED (toolbox/stage.rkt); the store-shaped `opt` generation they
;; succeeded was RETIRED 2026-07-12 (snapshot in toolbox/old/algebra-store-shaped.rkt).
;; stage-get/-view/-set/-update and reading/writing drive them, re-exported for callers.
;; EVERY WRITE NAVIGATES (every put goes through the lift); guide-AGNOSTIC. Machine,
;; pipeline, and the printing/lift rationale: scribble/zipper-core.scrbl.

(require racket/match
         "rope-core.rkt"
         "toolbox/main.rkt"
         (submod "toolbox/algebra.rkt" experimental))   ; the curried `lambda`, for reading/writing consumers

(provide
 (contract-out
  [start        (-> smr/c rope? guide/c guide/c zipper?)]
  [to-root      cmd/c]
  [zipper-guide/g procedure?]                ; the staged (g*) optics -- bare stages
  [zipper-guides  procedure?]                ; the staged both-edges guide optic (list-valued)
  [zipper-focus/g procedure?]
  [edge-view    (-> (or/c 0 1) (-> zipper? any))])
 compose-stage enter recompose               ; the stage ops (stage.rkt), for the optics
 stage-get stage-view stage-get* stage-set stage-update
 reading writing)

;; dev tooling -- NOT the navigation/editing API; reach via (require (submod "zipper-core.rkt" internal)).
;; run-chain's contract guards direct calls only; chain's expansion stays module-internal.
(module+ internal
  (provide chain
           (contract-out
            [run-chain (-> zipper? (listof (cons/c any/c cmd/c)) zipper?)])))

;; ---------- the machine: head, peek, navigate pipeline ----------

(struct head (before rope after) #:transparent)

(define (empty smr) ((make-rope smr)))

;; refocus a head onto a sub-rope given split: rope -> (values ls m rs). Returns the descended
;; head and the put-back -- a crumb (head -> head) the stack keeps to rebuild this level.
(define ((peek smr) split h)
  (match-define (head b t a) h)
  (define-values (ls m rs) (split t))
  (define focus (head (smr b ls) m (smr rs a)))
  (define (put h*) (head b ((make-rope smr) ls (head-rope h*) rs) a))
  (values focus put))

;; climb one level; unchanged once the focus contains the segment -- ascend's fixpoint halt.
(define (rise smr gs ge)
  (define (contains? h)
    (match-let ([(head b t a) h])
      (and (not (negative? (gs b (smr t a))))      ; start not left of the focus's left edge
           (not (positive? (ge (smr b t) a))))))   ; end not right of the focus's right edge
  (lambda (h k)
    (if (or (null? k) (contains? h))
        (values h k)
        (values ((car k) h) (cdr k)))))

;; descend one level; unchanged at a straddle/gap/boundary -- descend's fixpoint halt.
(define ((toward smr gs0 ge0) h k)
  (match-let*-values ([((head b t a))   h]
                      [(fr)             (frame smr b a)]    ; framed: read within-focus
                      [(gs)             (fr gs0)]
                      [(ge)             (fr ge0)]
                      [(mt)             (empty smr)]
                      [(lt rt)          ((multisect smr) t)]                  ; the balance halve
                      [(atom?)          (or (equal? lt mt) (equal? rt mt))]   ; an empty half, either side
                      [(into)           (lambda (split)
                                          (let-values ([(focus put) ((peek smr) split h)])
                                            (values focus (cons put k))))])
    (if atom?
        (values h k)
        (match* ((gs mt t) (gs lt rt) (ge lt rt) (ge t mt))  ; left edge | seam | seam | right edge
          [(-1 _ _ _) (error 'toward "start precedes the focus -- ascend further")]
          [(_ _ _ 1)  (error 'toward "end follows the focus -- ascend further")]
          [(_ 1 _ _)  (into (lambda (_) (values lt rt mt)))] ; whole seg right of seam -> lt | rt | ()
          [(_ _ -1 _) (into (lambda (_) (values mt lt rt)))] ; whole seg left  of seam -> () | lt | rt
          [(_ _ _ _)  (values h k)]))))                      ; straddle / gap / boundary -> halt

(define (navigate smr gs ge)
  (define ascend  (fixed (rise   smr gs ge) eq? (arg 0)))    ; rise   to a fixpoint
  (define descend (fixed (toward smr gs ge) eq? (arg 0)))    ; toward to a fixpoint
  (define (uncrossed h k)                                    ; reject a crossed cursor
    (match-define (head b t a) h)
    (define fr (frame smr b a))                              ; framed: read within-focus
    (let-values ([(ls rs) ((multisect smr (fr gs)) t)])
      (when (negative? ((fr ge) ls rs))
        (error 'navigate "crossed cursor -- end precedes start")))
    (values h k))
  (define (carve h k)                                        ; the exact cut
    (match-define (head b _ a) h)
    (define fr (frame smr b a))
    (let-values ([(focus put) ((peek smr) (multisect smr (fr gs) (fr ge)) h)])
      (values focus (cons put k))))
  (compose carve descend uncrossed ascend))

;; ---------- the public surface ----------

(struct zipper (smr gs ge hd stack) #:transparent         ; fixed leads -- reseal is (curry zipper smr gs ge)
                                                          ; field `hd` (not `head`): frees zipper-head for the head lens
  #:property prop:custom-write (lambda (z port mode) (zipper-show z port)))

;; contracts -- defined here, below the struct, because they mention zipper?.
(define smr/c        procedure?)
(define guide/c      procedure?)       ; shape only; the -1/0/1 codomain is enforced where a guide
                                       ; is called (rope-core's multisect)
(define cmd/c        (-> zipper? zipper?))

;; start: a fresh zipper -- whole rope as focus, cursor installed but not yet navigated.
(define (start smr rope gs ge) (zipper smr gs ge (head (smr "") rope (smr "")) '()))

;; zipper-lift: compose the ops (rightmost first) with navigate fixed as the permanent last op,
;; then reseal -- so every write lands where the guides point. No ops = plain re-navigation.
(define ((zipper-lift . ops) z)
  (match-define (zipper smr gs ge h k) z)
  ((apply compose (curry zipper smr gs ge)              ; curry reseals -- no cut
          (map (pass smr gs ge) (cons navigate ops)))   ; pass threads each op the (smr gs ge)
   h k))

;; ---------- the staged (g*) optics ----------
;; The optics, in the STAGED protocol (toolbox/stage.rkt): a stage is a bare function
;; ((f . idxs) . ws) -> (values g* put), driven by stage-get/stage-view/stage-set/
;; stage-update (and reading/writing). All are plain stages (no config of their own),
;; so `pure`-lifted. They put the flank CONTEXT on the render bus: a downstream
;; split/narrow reads it as its configuration.

;; zipper-focus/g: the focus rope focal, its flanking SUMMARIES (b a) on the bus.
(define zipper-focus/g
  (pure (lambda (z)
          (match-define (zipper smr gs ge (head b t a) k) z)
          (values (lambda (c) ((c b a) t))                 ; g*: renders (b a), then focus t
                  (lambda (new)
                    ((zipper-lift) (zipper smr gs ge (head b ((make-rope smr) new) a) k)))))))

;; zipper-guide/g: PARAMETERIZED by the edge -- (zipper-guide/g i) is the stage onto
;; edge i's guide (0 = start, 1 = end), focal, with edge i's cut (L R) on the render
;; bus (exactly what (edge-view i) reads). The put installs a new guide at edge i,
;; the other edge untouched, and re-navigates.
(define (zipper-guide/g i)
  (pure (lambda (z)
          (match-define (zipper smr gs ge (and h (head b t a)) k) z)
          (define-values (L R) (if (zero? i)
                                   (values b (smr t a))       ; edge 0's cut (start)
                                   (values (smr b t) a)))     ; edge 1's cut (end)
          (values (lambda (c) ((c L R) (if (zero? i) gs ge)))
                  (lambda (g*) ((zipper-lift)
                                (zipper smr (if (zero? i) g* gs) (if (zero? i) ge g*) h k)))))))

;; zipper-guides: the guide LIST focal (both edges, gs then ge), with each edge's cut on
;; the render bus as PARALLEL LISTS -- Ls = (L0 L1), Rs = (R0 R1), where (Li Ri) is edge
;; i's cut (what (edge-view i) reads). The both-edges optic (zipper-guide/g's list-valued
;; twin): the put installs both guides, navigates once; a transform maps cuts to guides.
(define zipper-guides
  (pure (lambda (z)
          (match-define (zipper smr gs ge (and h (head b t a)) k) z)
          (define-values (L0 R0) (values b (smr t a)))       ; edge 0's cut (start)
          (define-values (L1 R1) (values (smr b t) a))       ; edge 1's cut (end)
          (values (lambda (c) ((c (list L0 L1) (list R0 R1)) (list gs ge)))
                  (lambda (p) ((zipper-lift) (zipper smr (first p) (second p) h k)))))))

;; edge-view: edge i's cut as its two side-summaries -- a VIEWER (a pure read, no
;; put half): ((edge-view i) z) = (values L R). Fold it with (compose k (edge-view i)),
;; k receiving L R; splice it onto a stage's bus with render-with.
(define ((edge-view i) z)
  (match-define (zipper smr _ _ (head b m a) _) z)
  (if (zero? i)
      (values b (smr m a))
      (values (smr b m) a)))

;; to-root: fold every crumb back into the head -- the focus becomes the whole document.
;; Outside the lift: homing must not navigate back down; the guides survive.
(define (to-root z)
  (match-define (zipper smr gs ge h k) z)
  (zipper smr gs ge (foldl (lambda (crumb h) (crumb h)) h k) '()))

;; ---------- printing ----------

;; prints as the marked document: gap -> before‸after, seg -> before⟦focus⟧after. pieces re-cut from
;; the root with the guides, so it leans on the guide-focus alignment every write keeps (see scribble).
(define (zipper-show z port)
  (match-define (list gs ge) ((stage-get zipper-guides) z))
  (define smr  (zipper-smr z))
  (define root ((stage-get zipper-focus/g) (to-root z)))
  (let-values ([(b m a) ((multisect smr gs ge) root)])
    (if (equal? m (empty smr))
        (fprintf port "~a‸~a" b a)
        (fprintf port "~a⟦~a⟧~a" b m a))))

;; ---------- editing traces ----------

;; chain: pipe z0 through the commands, printing each command's source beside the zipper it
;; produces. The macro captures the source (only a macro can); run-chain threads it.
(define (run-chain z0 steps)
  (printf "~a~a\n" (~a "(start)" #:min-width 30) z0)
  (for/fold ([z z0]) ([step (in-list steps)])
    (match-define (cons label cmd) step)
    (define z* (cmd z))
    (printf "~a~a\n" (~a (~v label) #:min-width 30) z*)
    z*))
(define-syntax-rule (chain z0 op ...)
  (run-chain z0 (list (cons (quote op) op) ...)))

(module+ test
  (require rackunit)
  (define cc (make-summary string-length +))
  (define ((at n) L R) (cond [(< L n) 1] [(> L n) -1] [else 0]))
  (define (gap n)   (list (at n) (at n)))
  (define (seg i j) (list (at i) (at j)))
  (define (gap? z)  (equal? ((stage-get zipper-focus/g) z) ((make-rope cc))))
  (define (doc z)   (~a ((stage-get zipper-focus/g) (to-root z))))
  (define (place rope p) (start cc rope (first p) (second p)))   ; place a cursor list, unnavigated
  (define delete ((stage-set zipper-focus/g) ""))
  (define rope ((make-rope cc) "hello world"))
  (define g0 (gap 0))
  (define z0 (place rope g0))

  (check-equal? ((stage-get zipper-guides) z0) g0)
  (let ([g5 (gap 5)])
    (check-equal? ((stage-get zipper-guides) (((stage-set zipper-guides) g5) z0)) g5))

  ;; --- the staged (g*) optics, cross-checked among themselves + hardcoded expectations ---
  (let ([z (((stage-set zipper-guides) (seg 0 5)) z0)])         ; focus "hello", after " world"
    ;; zipper-focus/g: get = the focus rope; view = the flank SUMMARIES (b a)
    (check-equal? (~a ((stage-get zipper-focus/g) z)) "hello")
    (let-values ([(bs as) ((stage-view zipper-focus/g) z)])
      (check-equal? (list bs as) (list 0 6)))               ; cc = length: "" and " world"
    (check-equal? (doc (((stage-set zipper-focus/g) "HI") z)) "HI world")   ; the put installs & re-navigates
    (check-equal? (doc ((stage-update zipper-focus/g
                          (lambda (fr _b _a) ((make-rope cc) "[" fr "]"))) z))
                  "[hello] world")
    (check-equal? (doc (recompose zipper-focus/g z)) (doc z))  ; lawful lens: recompose = id
    ;; (zipper-guide/g i): edge i's guide focal, its cut (L R) on the bus
    (check-eq? ((stage-get (zipper-guide/g 0)) z) (first ((stage-get zipper-guides) z)))  ; the start guide
    (check-eq? ((stage-get (zipper-guide/g 1)) z) (second ((stage-get zipper-guides) z))) ; the end guide
    (let-values ([(L0 R0) ((stage-view (zipper-guide/g 0)) z)]
                 [(L1 R1) ((stage-view (zipper-guide/g 1)) z)])
      (check-equal? (list (cc L0) (cc L1)) '(0 5)))          ; start at 0, end after "hello"
    ;; install a new guide at ONE edge, the other untouched -- end -> 11 extends the seg
    (check-equal? (~a ((stage-get zipper-focus/g) (((stage-set (zipper-guide/g 1)) (at 11)) z)))
                  "hello world")
    ;; zipper-guides: the PAIR focal, both edges' cuts on the bus as parallel lists (Ls Rs)
    (check-equal? ((stage-get zipper-guides) z)                                   ; get = the pair
                  (list ((stage-get (zipper-guide/g 0)) z) ((stage-get (zipper-guide/g 1)) z)))
    (let-values ([(Ls Rs) ((stage-view zipper-guides) z)])
      (check-equal? (map cc Ls) '(0 5))                     ; the L of each edge's cut
      (check-equal? (map cc Rs) '(11 6)))                   ; the R of each edge's cut
    (check-equal? (doc (((stage-set zipper-guides) (seg 0 11)) z)) "hello world")   ; set installs both, navigates once
    (check-equal? (~a ((stage-get zipper-focus/g)                      ; update maps cuts -> guides
                       ((stage-update zipper-guides
                          (lambda (gs Ls Rs) (list (first gs) (at 11)))) z)))
                  "hello world"))

  (let ([z (((stage-set zipper-guides) (gap 5)) z0)])
    (check-true  (gap? z))
    (check-equal? (~a ((stage-get zipper-focus/g) z)) "")
    (check-equal? (doc (((stage-set zipper-focus/g) "XYZ") z)) "helloXYZ world"))

  (let ([z (((stage-set zipper-guides) (seg 0 5)) z0)])
    (check-false (gap? z))
    (check-equal? (~a ((stage-get zipper-focus/g) z)) "hello")
    (let ([z* (((stage-set zipper-focus/g) "HI") z)])
      (check-equal? (~a ((stage-get zipper-focus/g) z*)) "HI wo")
      (check-equal? (doc z*) "HI world"))
    (check-equal? (doc (delete z)) " world"))

  (let* ([z  (((stage-set zipper-guides) (seg 0 5)) z0)]
         [z* (((stage-set (compose-stage zipper-guides (as-stage (stage-lref 1 focal/g)))) (at 11)) z)])  ; one edge via the row lift
    (check-equal? (~a ((stage-get zipper-focus/g) z*)) "hello world"))

  ;; edge-view reads edge i's cut as (values L R) straight off the zipper -- a viewer;
  ;; fold it with (compose k (edge-view i)) -- k = list collects, a 2-arg k picks.
  (let ([z (((stage-set zipper-guides) (seg 0 5)) z0)])          ; focus "hello", before "", after " world"
    (check-equal? (~a ((stage-get zipper-focus/g) z)) "hello")                ; the focus rope
    (check-equal? ((compose list (edge-view 0)) z) '(0 11))   ; start: 0 | "hello world"
    (check-equal? ((compose list (edge-view 1)) z) '(5 6))    ; end:   "hello" | "world"
    (check-equal? ((compose (lambda (L R) L) (edge-view 0)) z) 0)  ; k receives L R -> L
    (check-equal? ((compose (lambda (L R) R) (edge-view 1)) z) 6))

  (let ([z (((stage-set zipper-guides) (seg 6 11)) z0)])
    (check-equal? (~a ((stage-get zipper-focus/g) z)) "world")
    (check-equal? (doc ((compose (writing (lambda ((_b _a) m) ((make-rope cc) "[" m "]")))
                                 (enter z)) zipper-focus/g))
                  "hello [world]"))

  (check-equal? (~a ((stage-get zipper-focus/g) ((compose to-root
                                                      ((stage-set zipper-focus/g) "HI")
                                                      ((stage-set zipper-guides) (seg 0 5))) z0)))
                "HI world")

  (let ([z (((stage-set zipper-guides) (seg 6 11)) z0)])
    (check-equal? ((compose list (edge-view 0)) z) '(6 5))
    (check-equal? ((compose list (edge-view 1)) z) '(11 0)))
  (let ([z (((stage-set zipper-guides) (gap 5)) z0)])              ; a gap: both edges read alike
    (check-equal? ((compose list (edge-view 0)) z) ((compose list (edge-view 1)) z)))

  (check-equal? (~a (((stage-set zipper-guides) (gap 5)) z0)) "hello‸ world")
  (check-equal? (~a (((stage-set zipper-guides) (seg 0 5)) z0)) "⟦hello⟧ world")
  (check-equal? (~a z0) "‸hello world")
  (check-equal? (~a (((stage-set zipper-focus/g) "HI") (((stage-set zipper-guides) (seg 0 5)) z0)))
                "⟦HI wo⟧rld")
  (let ([z (((stage-set zipper-guides) (gap 6)) (place ((make-rope cc) "ab\ncd\nef") (gap 0)))])
    (check-equal? (~a z) "ab\ncd\n‸ef"))

  (check-exn #rx"crossed cursor" (lambda () (((stage-set zipper-guides) (seg 5 2)) z0)))
  (check-not-exn (lambda () (((stage-set zipper-guides) (gap 5)) z0)))
  (check-not-exn (lambda () (((stage-set zipper-guides) (seg 2 5)) z0))))
