#lang racket

;; Stage: the staged / church-store optic -- a leaner successor to algebra.rkt's
;; store-shaped `opt`. A STAGE is a bare function, no struct:
;;
;;     ((f . idxs) . ws)  ->  (values g* put)
;;
;;   g*  : the CHURCH STORE -- (g* k) = ((k . rnds) . foci): it feeds its consumer
;;         the RENDERS first (a stage's configuration bus), then the FOCI (its
;;         worlds). Renders and foci are the two channels; the put path never sees
;;         the renders.
;;   put : news -> ws'   -- the write-back.
;;
;; Two curried application stages -- idxs (the configuration, from the previous
;; stage's renders) then ws (the world) -- so a plain stage is (pure world-fn) and
;; a configured stage binds its config in a parenthesised head via the experimental
;; curried `lambda`: (lambda ((cfg ...) w ...) ...).
;;
;; The whole calculus is application + composition:
;;   FORWARD  the bus is application -- (g1 f2): the next stage IS the consumer of
;;            the previous store, so composition is `(g1 f2)` plus threading puts.
;;   BACKWARD the puts thread by `compose` (values flow through it untouched).
;; The projections are consumers fed to g*, each ONE combinator:
;;   (g* (pure values)) = get      (g* pure) = view      (g* (pure put)) = recompose
;; -- `pure` on both sides: `pure` itself holds the renders (view); `(pure X)` skips
;; them and does X to the foci (get, recompose, update). LAW: (g* (pure put)) is the
;; recompose -- the identity for a lawful stage, the normalization e for a lossy one.
;;
;; The surface:
;;   compose-stage      stages, outer to inner; () = identity-stage
;;   identity-stage     the unit -- passes its indexes and worlds through
;;   enter              a world church-encoded: ((enter z) f) = (values g* put),
;;                      the pipeline entry ("no indexes first")
;;   recompose          (recompose f w ...) = the bare run -- the stage's own e
;;   stage-get stage-view stage-get*        the reads (foci / renders / both)
;;   stage-set stage-update                 the writes
;; Narrative and the derivation (why the peek is half an iso, why the renders lead,
;; the walk-count that motivated the single read): the design note.

(require "algebra.rkt"   ; pure, pass  (compose is racket's)
         (only-in (submod "algebra.rkt" experimental) lambda* staged-apply))  ; policies are curried stages

(provide compose-stage compose-stage2 identity-stage
         enter recompose
         stage-get stage-view stage-get* stage-set stage-update
         iso->stage spl->stage ldiag/g
         focal/g stage-lref stage-ldiag stage-list render-with as-stage
         reading writing)

;; helper: run a function and reify its multiple values as a list (algebra keeps a
;; private copy; stage.rkt is self-contained above the toolbox aggregator).
(define (value-list f . args) (call-with-values (lambda () (apply f args)) list))
(define (transpose ls) (if (null? ls) '() (apply map list ls)))   ; the parallel-lists <-> rows swap

;; ---------- composition: the bus is application, the puts compose ----------
;; compose-stage2 b1 b2: run b1 at OUR indexes and the world, hand its store g1 the
;; next stage b2 (whose indexes are b1's renders, whose worlds are b1's foci), and
;; thread the two puts. The composite is itself a stage -- `lambda idxs` receives
;; ITS configuration from further out (empty at the entry).
(define ((compose-stage2 b1 b2) . idxs)
  (lambda ws
    (define-values (g1 put1) (apply (apply b1 idxs) ws))
    (define-values (g2 put2) (g1 b2))           ; the one load-bearing line
    (values g2 (compose put1 put2))))           ; compose threads the values

;; identity-stage: the unit -- foci = its worlds, renders = its indexes, put = values.
(define (identity-stage . idxs)
  (lambda ws (values (lambda (k) (apply (apply k idxs) ws)) values)))

(define (compose-stage . bs)
  (foldl (lambda (b acc) (compose-stage2 acc b)) identity-stage bs))

;; ---------- entry: a world, church-encoded ----------
;; (enter z) is a degenerate store -- no renders, the world focal: ((enter z) k) =
;; ((k) z). Applying it to a stage IS the pipeline entry, so "store applies consumer"
;; is the single interaction at every level (entry, between stages, projection).
(define (enter . ws) (compose (apply pass ws) (pass)))

;; ---------- projections: consumers fed to the store ----------
;; each opens the stage at no indexes (one read) and feeds g* a terminal consumer.
(define ((stage-get f) . ws)                    ; foci as values
  (define-values (g _put) ((apply enter ws) f))
  (g (pure values)))
(define ((stage-view f) . ws)                   ; renders as values
  (define-values (g _put) ((apply enter ws) f))
  (g pure))
(define ((stage-get* f) . ws)                   ; foci then renders
  (define-values (g _put) ((apply enter ws) f))
  (g (lambda rnds (lambda foci (apply values (append foci rnds))))))
(define ((stage-set f) . news)                  ; ignore g, feed the put
  (lambda ws
    (define-values (_g put) ((apply enter ws) f))
    (apply put news)))
(define ((stage-update f h) . ws)               ; h : (foci ... renders ...) -> news
  (define-values (g put) ((apply enter ws) f))
  (g (lambda rnds (lambda foci
       (apply (compose put h) (append foci rnds))))))
(define (recompose f . ws)                      ; the bare run = the stage's own e
  (define-values (g put) ((apply enter ws) f))
  (g (pure put)))

;; ---------- wearings: a pair worn as a stage ----------
;; iso->stage: an iso worn as a stage -- focus along `to` (as iso->opt views along
;; `to`), put = `from` (the original ignored -- that absence IS the iso), no renders.
;; The generalization of sexp-edit's index-of (guide <-> index); composes after a
;; stage exactly as iso->opt composed after an opt.
(define (iso->stage i)
  (pure (lambda (w) (values (lambda (c) ((c) (i w))) (iso-from i)))))

;; spl->stage: a spl worn as a stage -- view along `from` (the lossy half), put = `to`;
;; the spl->opt orientation, staged. PutGet = the spl law on the nose, recompose = e.
(define (spl->stage r)
  (pure (lambda (w) (values (lambda (c) ((c) ((spl-from r) w))) (spl-to r)))))

;; ---------- list lifts ----------
;; ldiag/g: the staged list-diagonal, a POLICY over (context-lists, world-lists) -- select
;; index i out of every ctx list (forwarded as this stage's renders) and every world list
;; (the foci); the put broadcasts each new value along its whole world list. lref/g's
;; collapsing twin, and opt-ldiag's shape with the parallel lists split into a bus half
;; (idxs) and a data half (ws). Lawful only when a written list's slots are already equal.
(define (ldiag/g i)
  (lambda* (idxs ws)
    (define (nth l) (list-ref l i))
    (values (lambda (c) (apply (apply c (map nth idxs)) (map nth ws)))
            (lambda news (apply values (map (lambda (l x) (make-list (length l) x)) ws news))))))

;; ---------- row lifts: a policy over parallel lists ----------
;; A row POLICY P is itself a curried stage: (staged-apply P ctx-vals world-vals) -> (values
;; g* put) -- so every lens wears the one shape. It decides a single row's store and
;; write-back. The lifts below apply P at one row (stage-lref/-ldiag) or every row
;; (stage-list), transposing the context lists (the bus) and world lists (the foci) into rows.
;; The staged opt-lref / opt-ldiag / opt-list, with the context on the bus, not extra foci.

;; focal/g: the canonical policy -- the first world value writable, the rest of the row
;; (its context, then any further worlds) on the bus; the put returns the one new value.
;; (Named /g because algebra's `focal` is already in scope here.)
(define focal/g
  (lambda* (ctx ws)
    (values (lambda (c) (apply (apply c (append ctx (cdr ws))) (list (car ws))))
            (lambda (x) x))))

;; stage-lref: run P at row n; P's foci/renders are the composite's, and the put writes P's
;; new row back at n in each WORLD list (length-safe -- an under-supplied row leaves slots).
(define (stage-lref n P)
  (lambda* (idxs ws)
    (define-values (g put) (staged-apply P (map (lambda (l) (list-ref l n)) idxs)
                                           (map (lambda (l) (list-ref l n)) ws)))
    (values g (lambda news
                (define row* (value-list (lambda () (apply put news))))
                (apply values (for/list ([l (in-list ws)] [x (in-list row*)]) (list-set l n x)))))))

;; stage-ldiag: stage-lref's collapsing twin -- each value P's put returns is BROADCAST along
;; its whole world list rather than written at n. Lawful only when a list's slots are equal.
(define (stage-ldiag n P)
  (lambda* (idxs ws)
    (define-values (g put) (staged-apply P (map (lambda (l) (list-ref l n)) idxs)
                                           (map (lambda (l) (list-ref l n)) ws)))
    (values g (lambda news
                (define row* (value-list (lambda () (apply put news))))
                (apply values (for/list ([l (in-list ws)] [x (in-list row*)]) (make-list (length l) x)))))))

;; stage-list: P at EVERY row -- the context/world lists transpose into rows, P runs per row,
;; and the composite fans each row's foci and renders back into parallel lists; the put
;; distributes news (parallel lists) per row. (stage-list P) over a k-world row consumes k lists.
(define (stage-list P)
  (lambda* (idxs ws)
    (define-values (gs puts)
      (for/lists (g p) ([cr (in-list (transpose idxs))] [wr (in-list (transpose ws))]) (staged-apply P cr wr)))
    (values
     (lambda (c)
       (apply (apply c (transpose (map (lambda (g) (value-list g pure)) gs)))
              (transpose (map (lambda (g) (value-list g (pure values))) gs))))
     (lambda newcols
       (apply values (transpose (map (lambda (p nr) (value-list (lambda () (apply p nr))))
                                     puts (transpose newcols))))))))

;; as-stage: bridge a row policy (lambda* (idxs ws) ...) into a COMPOSABLE stage.
;; compose-stage spreads the upstream renders/foci; as-stage re-collects them into the two
;; list arguments the policy wants, applying it curried, so (as-stage (stage-list P)) drops
;; into a compose-stage chain after a stage whose renders / foci are the context / world lists.
(define (as-stage policy)
  (lambda idxs (lambda ws (staged-apply policy idxs ws))))

;; render-with: splice (f foci) onto a stage's render bus -- the staged attach-viewer (the bus
;; IS the viewer channel now). A pure world-render riding LAST, read by stage-view / stage-get*.
(define (render-with stage f)
  (lambda idxs
    (lambda ws
      (define-values (g put) (apply (apply stage idxs) ws))
      (values (lambda (c) (g (lambda rnds (lambda foci
                (apply (apply c (append rnds (list (apply f foci)))) foci)))))
              put))))

;; reading / writing: hand the store a CURRIED consumer h (renders then foci, in the
;; store's own application order) rather than a standard projection -- compose either
;; onto (enter z): ((compose (reading h) (enter z)) optic). reading returns h's value;
;; writing feeds it to the put. (writing h) is stage-update's core, left to compose by hand.
(define ((reading h) g _put) (g h))
(define ((writing h) g put)  (put (g h)))

;; ============================================================================
(module+ test
  (require rackunit
           (submod "algebra.rkt" experimental))    ; the curried `lambda` for configured stages

  ;; a toy tower over integers, exercising the bus end to end:
  ;;   split10 (plain): world n -> focus (quotient n 10); RENDER (remainder n 10),
  ;;           which configures the next stage; put q -> q*10 (drops the remainder,
  ;;           so the stage is LOSSY -- recompose floors n to a multiple of 10).
  (define split10
    (pure (lambda (n)
            (values (lambda (c) ((c (remainder n 10)) (quotient n 10)))
                    (lambda (q) (* q 10))))))
  ;;   tag (configured by r): the focus q passes through; render = (list r sign q).
  (define tag
    (lambda ((r) q)
      (values (lambda (c) ((c (list r (if (negative? q) '- '+))) q))
              (lambda (q*) q*))))
  (define tower (compose-stage split10 tag))

  ;; --- reads: the data path is the foci; the bus rides the view (curried, like
  ;;     algebra's opt-get: (stage-get f) is a reusable reader over worlds) ---
  (check-equal? ((stage-get  tower) 47) 4)               ; quotient
  (check-equal? ((stage-view tower) 47) (list 7 '+))     ; r = remainder configured tag; sign of 4
  (check-equal? (call-with-values (lambda () ((stage-get* tower) 47)) list)
                (list 4 (list 7 '+)))                     ; foci then renders (tag's render is one list)

  ;; --- the config genuinely FLOWED: tag saw the remainder as its index ---
  (check-equal? ((stage-view tower) -53) (list -3 '-))   ; (remainder -53 10) = -3; q = -5 -> sign -

  ;; --- writes: the put path, renders never in it ---
  (check-equal? (((stage-set tower) 9) 47) 90)           ; put 9 -> 90, original ignored
  ;; update's h sees foci THEN renders (like opt-update); it returns the put's news
  (check-equal? ((stage-update tower (lambda (q _r) (add1 q))) 47) 50)  ; 4 -> 5 -> *10

  ;; --- recompose = e: floor to a multiple of 10, idempotent ---
  (check-equal? (recompose tower 47) 40)
  (check-equal? (recompose tower (recompose tower 47)) 40)

  ;; --- enter is a store: ((enter z) f) = (values g* put); one read serves all ---
  (define-values (g put) ((enter 47) tower))
  (check-equal? (g (pure values)) 4)                     ; get
  (check-equal? (g pure) (list 7 '+))                    ; view
  (check-equal? (g (pure put)) 40)                       ; recompose, by hand
  (check-equal? (put 9) 90)                              ; set, by hand

  ;; --- reading / writing: a curried store consumer composed by hand onto (enter z) ---
  (check-equal? ((compose (reading (lambda ((r) q) (list q r))) (enter 47)) tower)
                (list 4 (list 7 '+)))                    ; h : renders then foci, application order
  (check-equal? ((compose (writing (lambda ((r) q) (add1 q))) (enter 47)) tower) 50)  ; 4 -> 5 -> *10

  ;; --- iso->stage: an iso worn as a stage -- focus along `to`, put = from, no renders ---
  (define io (iso->stage (iso add1 sub1)))
  (check-equal? ((stage-get io) 10) 11)             ; to
  (check-equal? (call-with-values (lambda () ((stage-view io) 10)) list) '())  ; no renders
  (check-equal? (((stage-set io) 7) 99) 6)          ; put = from, original ignored
  (check-equal? (recompose io 10) 10)               ; lawful iso: recompose = id
  ;; composes after a stage the same way iso->opt did after an opt
  (check-equal? ((stage-get (compose-stage split10 (iso->stage (iso add1 sub1)))) 47) 5)  ; (quotient 47 10) -> +1

  ;; --- spl->stage: a spl worn as a stage -- view along `from` (lossy), put = to ---
  (define int<-real (spl exact->inexact (compose inexact->exact round)))
  (define rs (spl->stage int<-real))
  (check-equal? ((stage-get rs) 3.7) 4)                       ; from: the lossy read
  (check-equal? (((stage-set rs) 10) 3.7) 10.0)                ; put = to
  (check-equal? (recompose rs 3.7) 4.0)                        ; recompose = e = to.from
  (check-equal? ((stage-get rs) (((stage-set rs) 7) 3.7)) 7)   ; PutGet: the spl law

  ;; --- ldiag/g: the list-diagonal policy over (ctx-lists, world-lists) ---
  ;; applied by hand (a policy, not a store-composed stage): two ctx lists on the bus,
  ;; one world list; select index 1 out of each, the put broadcasts along the world.
  (define-values (gd putd) (staged-apply (ldiag/g 1) (list '(a b c) '(x y z)) (list '(1 2 3))))
  (check-equal? (gd (pure values)) 2)                                     ; foci: i-th of the world
  (check-equal? (call-with-values (lambda () (gd pure)) list) '(b y))     ; renders: i-th of each ctx
  (check-equal? (call-with-values (lambda () (putd 99)) list) '((99 99 99)))  ; broadcast per world

  ;; --- the row lifts: policies over (context-lists, world-lists); focal/g the canonical one ---
  ;; (curried policies, eliminated by hand with staged-apply -- not store-composed stages here)
  (let-values ([(g p) (staged-apply focal/g '(c1 c2) '(w1 w2))])
    (check-equal? (g (pure values)) 'w1)                       ; focal: the first world writable
    (check-equal? (value-list g pure) '(c1 c2 w2))             ; the rest of the row on the bus
    (check-equal? (p 'X) 'X))                                  ; put returns the one new value
  (let-values ([(g p) (staged-apply (stage-lref 1 focal/g) (list '(x y z) '(1 2 3)) (list '(a b c)))])
    (check-equal? (g (pure values)) 'b)                        ; row 1's focal world
    (check-equal? (value-list g pure) '(y 2)))                 ; row 1's context, on the bus
  (let-values ([(_g p) (staged-apply (stage-lref 1 focal/g) (list '(x y z)) (list '(a b c)))])
    (check-equal? (p 'B) '(a B c)))                            ; write P's new row back at 1
  (let-values ([(_g p) (staged-apply (stage-ldiag 0 focal/g) (list '(1 2 3)) (list '(a b c)))])
    (check-equal? (p 'X) '(X X X)))                            ; broadcast along the world list
  (let-values ([(g p) (staged-apply (stage-list focal/g) (list '(1 2)) (list '(a b)))])
    (check-equal? (g (pure values)) '(a b))                    ; foci: the world list, fanned back
    (check-equal? (value-list g pure) '((1 2)))                ; renders: the context list (one column)
    (check-equal? (p '(X Y)) '(X Y)))                          ; set: news as parallel lists
  ;; render-with: a pure render spliced onto the bus, riding last
  (let ([S (pure (lambda (w) (values (lambda (c) ((c 'R) w)) add1)))])
    (let-values ([(g p) (((render-with S list)) 5)])
      (check-equal? (value-list g pure) '(R (5)))              ; the original render, then (list foci)
      (check-equal? (g (pure values)) 5)))

  ;; as-stage: a flat policy composed into a compose-stage chain -- upstream emits the
  ;; context list on the bus and the world list as its focus; stage-list runs P per row.
  (let ([up (pure (lambda (_w) (values (lambda (c) ((c '(1 2)) '(a b))) values)))])
    (check-equal? ((stage-get (compose-stage up (as-stage (stage-list focal/g)))) 'z) '(a b))
    (check-equal? ((stage-view (compose-stage up (as-stage (stage-list focal/g)))) 'z) '(1 2)))

  ;; --- identity-stage is the unit of compose-stage ---
  (check-equal? ((stage-get (compose-stage)) 99) 99)
  (check-equal? ((stage-get (compose-stage split10)) 47) 4)   ; a singleton tower
  (check-equal? (recompose identity-stage 42) 42))
