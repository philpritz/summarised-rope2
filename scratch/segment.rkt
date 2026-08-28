#lang racket

;; SCRATCH -- the segmenting split (draft, exploratory; discussion: this session).
;;
;; A guide is a classifier  (bs fs as) -> tag | #f  over summary values:
;;   tag  the focus is homogeneous -- one piece, labelled tag
;;   #f   the focus straddles a boundary -- descend into its halves
;; (segment g) walks the rope top-down, classifying instead of cutting; the result
;; is the rope segmented into maximal same-tag runs, as (cons tag subrope) pairs,
;; adjacent equal? tags welded back together. Obligation: a guide must tag every
;; indivisible (single-char) piece -- that is what bounds the descent.
;;
;; Guides normalize their inputs through their own smr, so the same guide runs
;; over its plain summary or over any bundle containing it; guide-product is then
;; just "all tags or #f", and segmentation by a product is the coarsest common
;; refinement of the factors' segmentations.

(require racket/match
         (only-in "../toolbox/algebra.rkt" lexicographic)
         "../rope-core.rkt"
         (submod "../rope-core.rkt" internal)
         "../summaries/summaries.rkt"
         "../summaries/sexp-summary.rkt"
         (submod "../summaries/sexp-summary.rkt" internal)
         "../summaries/lisp-summary.rkt"
         (submod "../summaries/lisp-summary.rkt" internal)
         "../summaries/occur-summary.rkt")

(provide segment guide-product line-window line-guide lisp-run-guide cursor-at
         guide->sides guides->region linecol-at line-end-at leftmost-guide grid-at
         char-at flip-guide flip-linecol backspace
         spine-cmp lisp-slot-guide lisp-slot-guide+ nav-spine nav-index flip-sexp
         occur-guide
         (struct-out ah)
         tokhash-smr keyword-table atomhash-smr keywordize
         (struct-out guide/s) spec-of line-window/s grid-at/s lisp-slot-guide/s
         segment/memo covering-key)

;; ---------- rope plumbing (rope-split / rope-join are private to rope-core) ----------
(define (split t)                      ; a leaf splits as the simulated branch: halve the text
  (if (branch? t)
      (values (branch-left t) (branch-right t))
      (let* ([smr (rope-algebra t)] [s (leaf-text t)] [mid (quotient (string-length s) 2)])
        (values ((make-rope smr) (substring s 0 mid)) ((make-rope smr) (substring s mid))))))
(define (join l r) ((make-rope (rope-algebra l)) l r))
(define (indivisible? t) (and (leaf? t) (<= (string-length (leaf-text t)) 1)))

;; ---------- the segmenting split ----------
;; #:merge generalizes welding to a partial semigroup on tags: (merge a b) is
;; the welded piece's tag, #f refuses. Obligation: associative, definedness
;; included -- else the seglist would depend on tree shape. The default is the
;; degenerate case: equal tags weld, kept.
(define ((segment guide #:merge [merge (lambda (a b) (and (equal? a b) a))]) t)
  (define smr (rope-algebra t))
  (define (weld tag piece tail)                    ; adjacent pieces weld when tags merge
    (define m (and (pair? tail) (merge tag (caar tail))))
    (if m
        (cons (cons m (join piece (cdar tail))) (cdr tail))
        (cons (cons tag piece) tail)))
  (let go ([bs (smr "")] [t t] [as (smr "")] [tail '()])
    (cond
      [(zero? (rope-leaves t)) tail]
      [(guide bs (smr t) as) => (lambda (tag) (weld tag t tail))]
      [(indivisible? t) (error 'segment "guide refused an indivisible piece: ~v" (leaf-text t))]
      [else
       (define-values (l r) (split t))
       (go bs l (smr r as) (go (smr bs l) r as tail))])))

;; ---------- the product of guides ----------
;; Tag = the list of component tags; classify iff every factor classifies.
(define ((guide-product . guides) bs fs as)
  (let loop ([gs guides] [acc '()])
    (cond [(null? gs) (reverse acc)]
          [((car gs) bs fs as) => (lambda (tag) (loop (cdr gs) (cons tag acc)))]
          [else #f])))

;; ---------- views: guide transformers ----------
;; A view is nothing but a function guide -> guide: where it has something to say
;; it answers itself (its own tag out of view, #f at an edge it straddles), and
;; where it doesn't it passes the call through to the wrapped guide. No protocol,
;; no reserved answers -- passing through IS calling through. The wrapped guide
;; sees the TRUE bs/fs/as, so its tags stay absolute; out-of-view subtrees are
;; approved at their root (never descended) and weld into one piece per view tag.
;; Cost: the fine work is confined to the view, plus O(depth) at its edges.

;; the lines n..m (inclusive) viewport
(define (((line-window n m) g) bs fs as)
  (define b (linecol-smr bs))
  (define f (linecol-smr fs))
  (define lo (linecol-lines b))                              ; line of the focus's first char
  (define hi (+ lo (linecol-lines f)                         ; line of its last char --
                 (if (and (> (linecol-lines f) 0)            ;   a final newline still lies
                          (zero? (linecol-cols f))) -1 0)))  ;   on the line it ends
  (cond [(< hi n)                    'before]
        [(> lo m)                    'after]
        [(and (>= lo n) (<= hi m))   (g bs fs as)]           ; in view: the guide, untouched
        [else                        #f]))                   ; straddles an edge: descend

;; ---------- the line guide (over linecol-smr) ----------
;; Single-line: no newline, or exactly one and it is the final char -- a newline
;; belongs to the line it ends. Tag = the line number = lines before the focus.
(define (line-guide bs fs as)
  (define b (linecol-smr bs))
  (define f (linecol-smr fs))
  (and (or (zero? (linecol-lines f))
           (and (= (linecol-lines f) 1) (zero? (linecol-cols f))))
       (linecol-lines b)))

;; ---------- sexp navigation: spine indexes as guides (after sexp-edit.rkt) ----------
;; An index is a spine -- the per-level slot list, innermost-first. Each
;; component picks its own side of the cut it's compared against (front >= -1/2,
;; back <= -1), and the comparison is lexicographic OUTERMOST-first; a prefix
;; precedes its extension, so a cut deeper than the target sits right of it.
(define (component-cmp a b) (cond [(< a b) -1] [(> a b) 1] [else 0]))
(define (back-component? c) (< c -1/2))
(define (cut-cmp c fb) (component-cmp c (if (back-component? c) (cdr fb) (car fb))))
(define (spine-cmp front back ix)                ; -> +1 = target right of the cut
  ((lexicographic cut-cmp) (reverse ix) (reverse (map cons front back))))

;; ---------- the refined index: (spine, offset) ----------
;; A spine names slot boundaries only; the refinement adds a CHAR OFFSET into
;; the slot's atom, so mid-atom positions are first-class sexp targets. The
;; comparison: the spine decides as before, except for cuts INSIDE the target
;; slot's atom (head = spine head + 1/2, same outer path), where the offset
;; decides -- read off atomhash lengths: tlen(L) = chars since the atom start
;; (front family), llen(R) = chars to its end (back family). Offsets inside
;; strings inherit atomhash's char-class caveat (quotes and inner ws are
;; delimiters to it); exact for plain atoms.
(define ((lisp-slot-guide+ ix [k 0]) L R)
  (define-values (f b) (lisp-spines (lisp-smr L) (lisp-smr R)))
  (define v (spine-cmp f b ix))
  (define back? (back-component? (car ix)))
  (cond
    [(zero? k) v]
    [(and (equal? (if back? (car b) (car f)) (+ (car ix) 1/2))
          (zero? ((lexicographic cut-cmp) (reverse (cdr ix))
                                          (reverse (map cons (cdr f) (cdr b))))))
     (if back?
         (component-cmp (ah-llen (atomhash-smr R)) k)   ; more left to the end: cut is left
         (component-cmp k (ah-tlen (atomhash-smr L))))] ; target deeper in: cut is left
    [(zero? v) 1]                                ; flush at the slot start, k chars to go
    [else v]))

(define (lisp-slot-guide ix) (lisp-slot-guide+ ix 0))

;; read a cut as a refined index -- the navigation read. Integer heads are slot
;; boundaries (offset 0); a fractional head rounds by cut kind: leading ws names
;; the NEXT slot, mid-atom the CONTAINING one, with the offset read off tlen(L).
(define (nav-index L R)
  (define lv (lisp-smr L))
  (define rv (lisp-smr R))
  (define-values (f _b) (lisp-spines lv rv))
  (if (integer? (car f))
      (values f 0)
      (let* ([m    (arm-exit (aref lv 0))]
             [rval (force (arm-val (aref rv (mode->i m))))]
             [ws?  (eq? (sexp-head rval) 'ws)])
        (if ws?
            (values (cons (max 0 (ceiling (car f))) (cdr f)) 0)
            (values (cons (max 0 (floor (car f))) (cdr f))
                    (ah-tlen (atomhash-smr L)))))))

(define (nav-spine L R) (let-values ([(ix _k) (nav-index L R)]) ix))

;; ---------- the sexp flip ----------
;; Re-anchor a sexp cursor off the BACK: back HEAD, front path (the path is a
;; left-based name shared by both anchorings -- only the head flips, after
;; sexp-edit's base-right), and the offset flips from since-start to to-end
;; (llen of the right part; 0 at any boundary, where tlen(L) = 0 detects it).
;; Structure-true: it survives tail edits that change char counts but not form
;; counts, where the char and linecol flips drift.
(define (flip-sexp g rope)
  (define-values (l r) ((multisect (rope-algebra rope) g) rope))
  (define-values (f b) (lisp-spines (lisp-smr l) (lisp-smr r)))
  (define kb (if (zero? (ah-tlen (atomhash-smr l))) 0 (ah-llen (atomhash-smr r))))
  (define bh (let ([h (car b)])                  ; a mid-atom cut reads B+1/2: floor back
               (if (integer? h) h (floor h))))   ; to the slot's flush back name
  (lisp-slot-guide+ (cons bh (cdr f)) kb))

;; ---------- classic guides -> side factors ----------
;; A classic guide judges a CUT: (L R) -> -1 | 0 | 1, +1 = the target boundary is
;; right of the cut (rope-core's convention). Lift one into a product factor
;; tagging each piece by its side of the target: judge the focus's two edge cuts
;; -- target at or left of the start edge => the focus is wholly 'after; at or
;; right of the end edge => wholly 'before; strictly inside => descend, so the
;; segmentation cuts exactly where the guide points. smr picks the summary the
;; guide reads (and combines the edge contexts).
(define ((guide->sides smr g) bs fs as)
  (define b (smr bs)) (define f (smr fs)) (define a (smr as))
  (cond [(<= (g b (smr f a)) 0) 'after]
        [(>= (g (smr b f) a) 0) 'before]
        [else                                    ; target strictly inside the focus: descend --
         (define n (and (summary-part? fs)      ; UNLESS the focus is one char whose emission
                        (part->summary fs char-smr #f)))   ; contains the target (a quote is
         (if (equal? n 1) 'after #f)]))          ; gap+atom in one char): land at its start

;; ---------- two classic guides -> a region factor ----------
;; The old repo's zipper focus as tags: gl and gr name the region's two cuts,
;; and each piece tags by where it lies relative to the region between them --
;; 'before / 'focus / 'after; a piece straddling a cut descends. It is a
;; renaming of (guide-product (guide->sides smr gl) (guide->sides smr gr)),
;; fused. gl = gr degenerates to an empty focus (plain before/after, i.e.
;; guide->sides is the diagonal); crossed guides (gl right of gr) are a
;; precondition violation, not a silent empty focus. guide->sides' single-char
;; escape hatch is mirrored: a cut strictly inside ONE char's emission (a sexp
;; cut at a string's quote) resolves to the char's start, so gr inside ends the
;; region before the char, gl inside begins it there. Integer-position cuts
;; (char / grid) never reach the hatch.
(define ((guides->region smr gl gr) bs fs as)
  (define b (smr bs)) (define f (smr fs)) (define a (smr as))
  (define bf (smr b f)) (define fa (smr f a))
  (cond [(>= (gl bf a) 0) 'before]        ; ends at/left of the left cut
        [(<= (gr b fa) 0) 'after]         ; starts at/right of the right cut
        [(and (<= (gl b fa) 0)            ; starts at/right of the left cut and
              (>= (gr bf a) 0)) 'focus]   ;   ends at/left of the right cut
        [else                             ; straddles a cut: descend -- unless the
         (define n (and (summary-part? fs)         ; focus is one char whose emission
                        (part->summary fs char-smr #f)))   ; contains the cut
         (cond [(not (equal? n 1))                     #f]
               [(and (> (gr b fa) 0) (< (gr bf a) 0)) 'after]
               [else                                   'focus])]))

;; ---------- grid navigation (after the old lisp-view.rkt) ----------
(define ((linecol-at r c) L R)                   ; the cut at row r, column c
  (match-define (linecol _ l k) (linecol-smr L))
  (cond [(or (< l r) (and (= l r) (< k c))) 1]
        [(and (= l r) (= k c))              0]
        [else                              -1]))

(define ((line-end-at r) L R)                    ; the cut at the end of row r's TEXT
  (match-define (linecol _ l _) (linecol-smr L)) ;   (before its newline)
  (cond [(< l r) 1]
        [(> l r) -1]
        [else (if (zero? (linecol-head (linecol-smr R))) 0 1)]))

(define ((leftmost-guide . gs) L R)              ; Kleene and: the leftmost target wins
  (apply min (map (lambda (g) (g L R)) gs)))

;; editor-style (row . col): a column past the line's text lands at the line end
;; -- the leftmost of "column c" and "end of row r".
(define (grid-at r c) (leftmost-guide (linecol-at r c) (line-end-at r)))

;; ---------- guide flipping (after sexp-edit's re-anchoring) ----------
;; A cursor is usually named from the FRONT -- (char-at p) reads the chars
;; before the cut -- so an edit left of it shifts the name and the cursor must
;; be re-aimed after every keystroke. flip-guide re-anchors: split the text at
;; the guide, read the cut's BACK name (the chars after it), and return the
;; guide navigating by that. It names the SAME position on this text, but under
;; edits left of the cut it holds its ground: deletion at the cursor keeps it in
;; place, insertion at the cursor advances it past the new text -- editor
;; behaviour, with no cursor arithmetic per edit.
(define ((char-at p) L R)                        ; the classic front cursor
  (define n (char-smr L))
  (cond [(< n p) 1] [(> n p) -1] [else 0]))

(define (flip-guide g rope)                      ; the current guide + text -> the flipped guide
  (define-values (_l r) ((multisect (rope-algebra rope) g) rope))
  (define k (char-smr r))                        ; the back name: chars after the cut
  (lambda (L R)
    (define b (char-smr R))
    (cond [(> b k) 1] [(< b k) -1] [else 0])))

;; the linecol specialisation: the cut's back name in GRID terms, read straight
;; off the right part's linecol -- lines = rows after the cut, head = chars from
;; the cut to its line's newline. The flipped guide compares those, reversed-
;; lexicographic (more text after = further left). Where the char flip holds
;; "k chars from the end", this holds "kl rows from the end, kh short of the
;; line's end" -- a grid position that survives edits on earlier lines even when
;; they add or remove WHOLE LINES, which shifts every front (row . col) name.
(define (flip-linecol g rope)
  (define-values (_l r) ((multisect (rope-algebra rope) g) rope))
  (define v  (linecol-smr r))
  (define kl (linecol-lines v))
  (define kh (linecol-head v))
  (lambda (L R)
    (define w (linecol-smr R))
    (define lb (linecol-lines w))
    (define hb (linecol-head w))
    (cond [(or (> lb kl) (and (= lb kl) (> hb kh)))  1]
          [(and (= lb kl) (= hb kh))                 0]
          [else                                     -1])))

;; ---------- the cursor guide (over char-smr) ----------
;; A product factor for a cursor at gap position p (0..N, between chars): pieces
;; wholly left tag 'before, wholly right 'after; a focus straddling p descends,
;; so the segmentation cuts exactly at the cursor -- even mid-atom. Total: an
;; integer gap never falls strictly inside a single char.
(define ((cursor-at p) bs fs as)
  (define start (char-smr bs))
  (define end   (+ start (char-smr fs)))
  (cond [(<= end p)   'before]
        [(>= start p) 'after]
        [else         #f]))

;; ---------- the lisp guide (over lisp-smr; needs char-smr in the bundle too) ----------
;; Tags:
;;   (code    spine)  a code token (atom / bracket) plus its trailing whitespace;
;;                    spine = the floored front spine at the piece's left edge
;;   (string  spine)  a string literal: the sexp-index of its ~ atom -- a string IS
;;   (charlit spine)  an atom in the emitted domain, with slot and depth like any other
;;   (class   idx)    comment / block: emitted whitespace, NO sexp presence -- the
;;                    run index (boundaries strictly left, plus the one this piece's
;;                    class opens) is what keeps ;a\n;b apart
;; Spine rounding: a piece STARTING AT its construct leans before the ~ atom (round
;; up to its slot); a piece starting INSIDE straddles the begun atom (round down).
;; Every token step is a spine step, so spines subsume the run index everywhere the
;; construct has a sexp presence.

(define (run-index b-arm cl m0)
  (match-define (arm _ n _ tailc pend _) b-arm)
  (+ n (if (boundary? tailc cl (if pend 'code m0)) 1 0)))

(define (lisp-run-guide bs fs as)
  (define b (lisp-smr bs)) (define f (lisp-smr fs)) (define a (lisp-smr as))
  (define b-arm (aref b 0))
  (define m0 (arm-exit b-arm))                   ; entry mode at the focus
  (match-define (arm m1 n fc tailc pend _) (aref f (mode->i m0)))
  (define (after) (or (arm-first (aref a (mode->i m1))) 'code))
  (define cl
    (cond [(not fc)         #f]                  ; empty focus (unreachable from segment)
          [(> n 0)          #f]                  ; a lexical run boundary inside: descend
          [(eq? fc 'hash)   (after)]             ; all glue: the construct it adheres to
          [pend (if (boundary? tailc (after) 'code) #f fc)]  ; trailing glue: new run = straddle
          [else fc]))
  (define (front-spine L R round)                ; front spine at the cut L|R, head rounded
    (define-values (fr _) (lisp-spines L R))
    (cons (round (car fr)) (cdr fr)))
  (and cl
       (case cl
         [(code)
          (let ([s1 (front-spine b (lisp-smr f a) floor)]
                [s2 (front-spine (lisp-smr b f) a floor)]
                [hd (sexp-head (force (arm-val (aref f (mode->i m0)))))])
            ;; equal floored edges say "no spine excursion" -- EXCEPT a complete
            ;; form + trailing ws, whose after-ws edge floors back onto the form's
            ;; own slot. A bracket head betrays it: a code token starting with a
            ;; bracket can only be the lone bracket (the single-char case).
            (and (or (= 1 (char-smr fs))         ; indivisible: a lone bracket steps the spine
                     (and (equal? s1 s2)         ; token-homogeneous...
                          (not (memq hd '(open close)))))
                 (list 'code s1)))]
         [(string)                               ; inside: the ~ atom has begun -> floor
          (list 'string (front-spine b (lisp-smr f a)
                                     (if (memq m0 '(string escape)) floor ceiling)))]
         [(charlit)                              ; the ~ is emitted only at the payload: always up
          (list 'charlit (front-spine b (lisp-smr f a) ceiling))]
         [else (list cl (run-index b-arm cl m0))])))

;; ---------- backspace: an edit ON the tagged seglist ----------
;; Delete the char left of the cursor, working on segment output whose LAST
;; product factor is a cursor side ('before | 'after): trim the final 'before
;; piece by one char, reassemble the rope from the pieces (make-rope re-fuses
;; the seam), and recut with the cursor one left. The fresh cut re-derives every
;; syntax tag, so a deletion that changes lexical state -- killing a quote --
;; relabels everything downstream. make-guide : cursor -> guide rebuilds the
;; cursor'd guide at the new position. Returns (values rope segs); rope #f when
;; the cursor is at the document start (nothing to delete).
(define (backspace segs #:guide make-guide)
  (define (before? s) (eq? (last (car s)) 'before))
  (define-values (pre post) (splitf-at segs before?))
  (define cursor (for/sum ([s (in-list pre)]) (char-smr (cdr s))))
  (cond
    [(zero? cursor) (values #f segs)]
    [else
     (define smr     (rope-algebra (cdr (first segs))))
     (define lastp   (format "~a" (cdr (last pre))))
     (define trimmed (substring lastp 0 (sub1 (string-length lastp))))
     (define parts   (append (map cdr (drop-right pre 1))
                             (if (string=? trimmed "") '() (list trimmed))
                             (map cdr post)))
     (define rope*   (apply (make-rope smr) parts))
     (values rope* ((segment (make-guide (sub1 cursor))) rope*))]))

;; ---------- the occurrence guide (over occur-smr) ----------
;; Two tags off the word-agnostic occur summary: a piece is 'occur when every
;; one of its chars lies inside some occurrence of W in the WHOLE document
;; (matches may cross the piece's edges), 'plain when none does; anything mixed
;; descends. Adjacent or overlapping occurrences that tile a stretch come back
;; as one 'occur piece through descent + welding, so the guide needs only the
;; two decidable extremes:
;;   touched  -- occurrences intersecting the focus, by counting alone:
;;               count(b++f++a) - count(b) - count(a)   (0 => 'plain)
;;   covered  -- ONE match contains the whole focus: scan its possible
;;               alignments against the stored edge-crossers (posns), no text
;; A single char is covered iff touched -- that is what makes the guide total.
;; Cache caveat (see occur-summary.rkt): segment's context accumulation builds
;; fresh combined cells, so the shared trie only reuses work below stable
;; subtrees -- fine for scratch.
(define ((occur-guide W) bs fs as)
  (define mw (string-length W))
  (define B ((occur-smr bs) W))
  (define F ((occur-smr fs) W))
  (define A ((occur-smr as) W))
  (define lenB (wm-len B))
  (define lenF (wm-len F))
  (define lenA (wm-len A))
  (define touched (- (wm-count ((occur-smr bs fs as) W))
                     (wm-count B) (wm-count A)))
  (define (covered?)                     ; some single match contains the whole focus?
    (for/or ([q (in-range (- lenF mw) 1)])       ; q: its start relative to the focus
      (and (if (and (zero? q) (= mw lenF))
               (positive? (wm-count F))          ; exactly the focus: an interior match
               (memv q (wm-posns F)))            ; else one of F's own edge-crossers
           (or (zero? q)                         ; b agrees (or no overlap) -- and the match
               (and (>= (+ lenB q) 0)            ;   STARTS INSIDE the document: a window
                    (memv (+ lenB q) (wm-posns B))))   ;   hanging off the start is no match
           (or (= (+ q mw) lenF)                 ; a agrees (or no overlap) -- and the match
               (and (<= (+ (- q lenF) mw) lenA)  ;   ENDS INSIDE the document
                    (memv (- q lenF) (wm-posns A)))))))
  (cond [(zero? touched) 'plain]
        [(= lenF 1)      'occur]
        [(covered?)      'occur]
        [else            #f]))

;; ---------- the token hash + keyword retagging ----------
;; summaries.rkt's hash-smr (Karp-Rabin as a monoid) with ONE contrivance: a
;; whitespace char contributes the IDENTITY, so "  define \n" hashes as "define".
;; Safe here because a code piece is one token + ws by construction -- there is
;; never a second atom in the piece to fuse with.
(define tok-M (- (expt 2 61) 1))
(define tok-B 1000003)
(define (tok-leaf s)
  (for/fold ([h 0] [sc 1] #:result (fp h sc))
            ([c (in-string s)] #:unless (char-whitespace? c))
    (values (modulo (+ (* h tok-B) (char->integer c)) tok-M)
            (modulo (* sc tok-B) tok-M))))
(define (tok-join x y)
  (fp (modulo (+ (* (fp-h x) (fp-scale y)) (fp-h y)) tok-M)
      (modulo (* (fp-scale x) (fp-scale y)) tok-M)))
(define tokhash-smr (make-summary tok-leaf tok-join))

(define (keyword-table . kws)                    ; precomputed: token hash -> keyword
  (for/hash ([k (in-list kws)]) (values (tokhash-smr k) k)))

;; ---------- boundary-atom hashes: keyword-ness decided IN the guide ----------
;; A piece is only ever a FRAGMENT of its token (a cursor or line factor can cut
;; mid-atom), so tagging it kw needs the WHOLE atom's hash. atomhash-smr is the
;; boundary monoid that makes that local: its value carries the token hash of
;; the fragment's first and last atom runs (delimiters: ws, brackets, ", ;),
;; whether the fragment is one unbroken run, and its head class. Any piece then
;; reconstructs its atom as  trail(bs) ++ own ++ lead(as)  -- three cached
;; reads and O(1) fp joins. Caveat, documented: char-class atom boundaries,
;; so #|...|# glue abutting an atom with no whitespace misreads (false negative
;; only). The exact version would carry these fields inside lisp-smr's arms.
(define (atom-char? c)
  (not (or (char-whitespace? c) (memv c '(#\( #\) #\[ #\] #\{ #\} #\" #\;)))))
(define tok-id (fp 0 1))
(struct ah (head lead llen all? trail tlen) #:transparent)   ; head: 'empty | 'atom | 'delim
;; lead/trail: hashes of the first/last atom runs; llen/tlen their LENGTHS --
;; tlen(L) at a mid-atom cut = chars since the atom's start, llen(R) = chars to
;; its end: the two offsets the mid-atom index refinement reads.
(define (ah-leaf s)
  (define n (string-length s))
  (if (zero? n)
      (ah 'empty tok-id 0 #t tok-id 0)
      (let ([i (or (for/first ([k (in-range n)] #:unless (atom-char? (string-ref s k))) k) n)]
            [j (or (for/first ([k (in-range (sub1 n) -1 -1)]
                               #:unless (atom-char? (string-ref s k))) k) -1)])
        (ah (if (atom-char? (string-ref s 0)) 'atom 'delim)
            (tok-leaf (substring s 0 i)) i
            (= i n)
            (tok-leaf (substring s (add1 j))) (- n (add1 j))))))
(define (ah+ x y)
  (match-define (ah xh xl xll xa xt xtl) x)
  (match-define (ah yh yl yll ya yt ytl) y)
  (ah (if (eq? xh 'empty) yh xh)
      (if xa (tok-join xl yl) xl)                ; x all atom: its lead runs into y's
      (if xa (+ xll yll) xll)
      (and xa ya)
      (if ya (tok-join xt yt) yt)                ; y all atom: x's trail runs into it
      (if ya (+ xtl ytl) ytl)))
(define atomhash-smr (make-summary ah-leaf ah+))

;; the guide wrapper: retag operator-position code pieces whose WHOLE atom is a
;; keyword. kw is intrinsic in the tag -- no post-weld pass. A ws-only piece
;; (head 'delim) is the token's trailing space, never the keyword itself.
(define ((keywordize table) g)
  (lambda (bs fs as)
    (match (g bs fs as)
      [(list 'code (and spine (cons 0 (cons _ _))))
       (define b (atomhash-smr bs))
       (define f (atomhash-smr fs))
       (define whole
         (tok-join (ah-trail b)
                   (if (ah-all? f)
                       (tok-join (ah-lead f) (ah-lead (atomhash-smr as)))
                       (ah-lead f))))
       (if (and (eq? (ah-head f) 'atom) (hash-ref table whole #f))
           (list 'kw spine)
           (list 'code spine))]
      [r r])))

;; ---------- transparent guides ----------
;; A guide carrying its specification -- a datum -- callable as itself. The
;; contract that makes specs usable as memo keys: spec equality implies
;; extensional equality (impls must be pure functions of their spec). An
;; unwrapped guide's spec is the procedure itself: eq-keyed, conservative.
(struct guide/s (spec impl)
  #:property prop:procedure (struct-field-index impl))
(define (spec-of g) (if (guide/s? g) (guide/s-spec g) g))

(define (line-window/s n m g)                    ; the spec'd viewport
  (guide/s (list 'line-window n m (spec-of g)) ((line-window n m) g)))

;; spec'd CURSORS: classic guides carrying their address as the spec, so a
;; mode's movements can be guide -> guide functions (read the address off the
;; spec, build the moved guide) and its edit targets guide -> seg-guide.
(define (grid-at/s r c)
  (guide/s (list 'grid-at r c) (grid-at r c)))
(define (lisp-slot-guide/s ix [k 0])
  (guide/s (list 'lisp-slot ix k) (lisp-slot-guide+ ix k)))

;; ---------- the memoizing segment ----------
;; segment, with a memo on subtree seglists. The memoizer is deliberately dumb;
;; ALL intelligence lives in the key fn:
;;   key : (bs fs as spec) -> key | #f
;;   #f    don't memoize this call (straddles, tiny foci, opaque specs -- policy)
;;   value the canonical identity; equal values share one table entry, so a key
;;         fn that strips covering wrappers makes DIFFERENT viewports hit each
;;         other's entries.
(define ((segment/memo guide #:key key #:table [table (make-hash)] #:stats [stats #f]) t)
  (define smr  (rope-algebra t))
  (define spec (spec-of guide))
  (define (bump! what) (when stats (hash-update! stats what add1 0)))
  (define (weld tag piece tail)
    (match tail
      [(cons (cons tag2 p2) rest) #:when (equal? tag tag2)
       (cons (cons tag (join piece p2)) rest)]
      [_ (cons (cons tag piece) tail)]))
  (define (splice sub tail)                      ; a cached seglist onto the tail, welding the seam
    (foldr (lambda (sg acc) (weld (car sg) (cdr sg) acc)) tail sub))
  (let go ([bs (smr "")] [t t] [as (smr "")] [tail '()])
    (define (step tail)                          ; classify-or-descend, memo-blind
      (cond
        [(guide bs (smr t) as) => (lambda (tag) (weld tag t tail))]
        [(indivisible? t) (error 'segment/memo "guide refused an indivisible piece: ~v" (leaf-text t))]
        [else
         (define-values (l r) (split t))
         (go bs l (smr r as) (go (smr bs l) r as tail))]))
    (cond
      [(zero? (rope-leaves t)) tail]
      [else
       (define k (key bs (smr t) as spec))
       (cond
         [(not k) (step tail)]
         [(hash-ref table k #f) => (lambda (sub) (bump! 'hit) (splice sub tail))]
         [else (bump! 'miss)
               (define sub (step '()))
               (hash-set! table k sub)
               (splice sub tail)])])))

;; the default key for the line-window stack: strip COVERING windows (the
;; hereditary guard: the focus's whole line span inside the window -- true of a
;; rope implies true of every subrope, so pass-through holds all the way down),
;; then key the canonical tuple by full-content fingerprints (hash-smr must ride
;; the bundle). #f -- no memo -- on straddling / out-of-view foci and tiny ones.
(define ((covering-key [min-chars 4]) bs fs as spec)
  (and (>= (char-smr fs) min-chars)
       (let strip ([spec spec])
         (match spec
           [(list 'line-window n m inner)
            (define lo (linecol-lines (linecol-smr bs)))
            (define f  (linecol-smr fs))
            (define hi (+ lo (linecol-lines f)
                          (if (and (> (linecol-lines f) 0) (zero? (linecol-cols f))) -1 0)))
            (and (>= lo n) (<= hi m) (strip inner))]
           [inner (list (hash-smr bs) (hash-smr fs) (hash-smr as) inner)]))))

;; ============================================================================
(module+ main
  (define lisp-bundle (bundle char-smr lisp-smr))
  (define both-bundle (bundle char-smr linecol-smr lisp-smr))
  (define line*lisp   (guide-product line-guide lisp-run-guide))

  (define (show smr guide text)
    (printf "~n~s~n" text)
    (define segs ((segment guide) ((make-rope smr) text)))
    (unless (equal? text (apply string-append (map (lambda (s) (format "~a" (cdr s))) segs)))
      (error 'show "pieces do not concatenate back to the text"))
    (for ([seg (in-list segs)])
      (printf "  ~a  ~s~n" (~a (car seg) #:min-width 24) (format "~a" (cdr seg)))))

  (printf "========== lines (line-guide over linecol-smr) ==========~n")
  (for ([t (in-list (list "hello\nworld\nand more" "a\n" "a\n\nb"))])
    (show linecol-smr line-guide t))

  (printf "~n========== lisp (lisp-run-guide over char*lisp) ==========~n")
  (for ([t (in-list (list "(a \"x y\" b)"
                          "\"a\"\"b\""
                          "x ; c\ny"
                          "a#|x|#b"
                          "(f #\\( x)"
                          "(aa (p q) cc)"))])
    (show lisp-bundle lisp-run-guide t))

  (printf "~n========== lines * lisp (guide-product over char*linecol*lisp) ==========~n")
  (for ([t (in-list (list "(define (f x) ; twice\n  (g \"a\nb\" x))"))])
    (show both-bundle line*lisp t))

  (printf "~n========== keywordize: kw in the guide, via boundary-atom hashes ==========~n")
  (let* ([kwb  (bundle char-smr lisp-smr atomhash-smr)]
         [tbl  (keyword-table "define" "lambda" "if")]
         [text "(define (f x) (if x (g define) x))"]
         [g    (guide-product ((keywordize tbl) lisp-run-guide) (cursor-at 3))]
         [segs ((segment g) ((make-rope kwb) text))])
    (printf "~n~s   cursor at |3 -- INSIDE define; both halves must tag kw~n" text)
    (for ([seg (in-list segs)])
      (printf "  ~a  ~s~n" (~a (car seg) #:min-width 24) (format "~a" (cdr seg)))))

  (printf "~n========== cursor: lisp * (cursor-at 2) -- mid-atom cursor ==========~n")
  (let* ([text "(aa (p q) cc)"]
         [segs ((segment (guide-product lisp-run-guide (cursor-at 2)))
                ((make-rope lisp-bundle) text))])
    (printf "~n~s   cursor at |2 -- inside the atom aa~n" text)
    (for ([seg (in-list segs)])
      (printf "  ~a  ~s~n" (~a (car seg) #:min-width 28) (format "~a" (cdr seg)))))

  (printf "~n========== occur-guide: word occurrences as two tags ==========~n")
  (let ([ob (bundle char-smr occur-smr)])
    (define (show-occur W . texts)
      (define segs ((segment (occur-guide W)) (apply (make-rope ob) texts)))
      (printf "~n~s in ~s~n" W (apply string-append texts))
      (for ([seg (in-list segs)])
        (printf "  ~a  ~s~n" (~a (car seg) #:min-width 8) (format "~a" (cdr seg)))))
    (show-occur "needle" "a needle in needles, and a knee")
    (show-occur "needle" "ne" "edl" "e")                     ; one match across 3 leaves
    (show-occur "aa" "xx" "aaaa" "yy")                       ; overlapping matches tile + weld
    (show-occur "aaba" "abaaba"))                            ; regression: a W-window hanging off
                                                             ;   the doc START is NOT an occurrence

  (printf "~n========== region: two guides -> before/focus/after ==========~n")
  (let* ([text "(aa (p q) cc)"]
         [segs ((segment (guide-product lisp-run-guide
                                        (guides->region lisp-bundle (char-at 4) (char-at 9))))
                ((make-rope lisp-bundle) text))])
    (printf "~n~s   region = chars 4..9, the (p q) form~n" text)
    (for ([seg (in-list segs)])
      (printf "  ~a  ~s~n" (~a (car seg) #:min-width 28) (format "~a" (cdr seg)))))

  (printf "~n========== backspace: the recut updates the syntax ==========~n")
  (let* ([text "(aa \"xy\" bb)"]
         [mk   (lambda (c) (guide-product lisp-run-guide (cursor-at c)))]
         [segs ((segment (mk 8)) ((make-rope lisp-bundle) text))])
    (define (show-segs label segs)
      (printf "~n~a~n" label)
      (for ([seg (in-list segs)])
        (printf "  ~a  ~s~n" (~a (car seg) #:min-width 28) (format "~a" (cdr seg)))))
    (show-segs (format "~s, cursor at 8 -- right after the closing quote:" text) segs)
    (define-values (r1 segs1) (backspace segs #:guide mk))
    (show-segs (format "backspace (kills the quote) -> ~s:" (format "~a" r1)) segs1)
    (define-values (r2 segs2) (backspace segs1 #:guide mk))
    (show-segs (format "backspace again -> ~s:" (format "~a" r2)) segs2))

  (printf "~n========== flip-guide: one back-anchored cursor across edits ==========~n")
  (let* ([text "(aa \"xy\" bb)"]
         [doc  ((make-rope lisp-bundle) text)]
         [gb   (flip-guide (char-at 8) doc)]     ; flipped ONCE; never touched again
         [cut  (segment (guide-product lisp-run-guide (guide->sides char-smr gb)))])
    (define (show-segs label segs)
      (printf "~n~a~n" label)
      (for ([seg (in-list segs)])
        (printf "  ~a  ~s~n" (~a (car seg) #:min-width 28) (format "~a" (cdr seg)))))
    (define (before? s) (eq? (last (car s)) 'before))
    (define (rebuild segs edit)                  ; edit : before-piece list -> piece list
      (define-values (pre post) (splitf-at segs before?))
      (apply (make-rope lisp-bundle) (append (edit (map cdr pre)) (map cdr post))))
    (show-segs "cursor at 8 from the front = 4 from the back:" (cut doc))
    (define doc1 (rebuild (cut doc)              ; backspace: kill the quote
                          (lambda (ps)
                            (define t (format "~a" (last ps)))
                            (append (drop-right ps 1)
                                    (list (substring t 0 (sub1 (string-length t))))))))
    (show-segs (format "backspace -> ~s -- SAME guide:" (format "~a" doc1)) (cut doc1))
    (define doc2 (rebuild (cut doc1)             ; insert: type zz at the cursor
                          (lambda (ps) (append ps (list "zz")))))
    (show-segs (format "insert \"zz\" -> ~s -- SAME guide, cursor past it:" (format "~a" doc2))
               (cut doc2)))

  (printf "~n========== flip-linecol: a grid cursor surviving a NEW LINE above ==========~n")
  (let* ([text "(aaa\n bb x\n cc)"]
         [doc  ((make-rope both-bundle) text)]
         [gb   (flip-linecol (grid-at 1 4) doc)] ; row 1 col 4: between "bb " and "x"
         [cut  (lambda (d) ((segment (guide-product line-guide lisp-run-guide
                                                    (guide->sides linecol-smr gb))) d))])
    (define (show-segs label segs)
      (printf "~n~a~n" label)
      (for ([seg (in-list segs)])
        (printf "  ~a  ~s~n" (~a (car seg) #:min-width 28) (format "~a" (cdr seg)))))
    (show-segs "grid (1 . 4), flipped = 1 row from the end, 1 short of its EOL:" (cut doc))
    ;; split line 0 in two: every front (row . col) below it is now stale
    (define doc1 ((make-rope both-bundle) "(a\naa\n bb x\n cc)"))
    (show-segs "line 0 split in two above it -- SAME guide, cursor still before x:" (cut doc1)))

  (printf "~n========== refined sexp index + the sexp flip ==========~n")
  (let* ([B    (bundle char-smr lisp-smr atomhash-smr)]
         [mk   (make-rope B)]
         [text "(aa (p q) cc)"]
         [land (lambda (g txt) (let-values ([(l _r) ((multisect B g) (mk txt))]) (char-smr l)))])
    (printf "~n~s~n" text)
    (printf "  slot (2 0) + offset 1 -- mid cc  -> char ~a~n" (land (lisp-slot-guide+ '(2 0) 1) text))
    (define g0 (lisp-slot-guide '(1 0)))
    (printf "  slot (1 0) -- before (p q)       -> char ~a~n" (land g0 text))
    (define gbs (flip-sexp  g0 (mk text)))
    (define gbc (flip-guide g0 (mk text)))
    (define text2 "(aa (p q) cccc)")             ; the tail atom grows: chars change, forms don't
    (printf "  tail edit ~s:~n" text2)
    (printf "    flip-sexp  -> char ~a   (structure-true: holds)~n" (land gbs text2))
    (printf "    flip-guide -> char ~a   (char back-name: drifts)~n" (land gbc text2)))

  (printf "~n========== memo: shifted viewports sharing cache entries ==========~n")
  (let* ([buf   (bundle char-smr linecol-smr lisp-smr hash-smr)]
         [inner (guide/s 'lines*lisp (guide-product line-guide lisp-run-guide))]
         ;; 8 forms x 16 chars = 128 chars: the 32-char leaves align with 4-line groups
         [text  (apply string-append
                       (for/list ([t '((a b c) (d e f) (g h i) (j k l)
                                       (m n o) (p q r) (s t u) (v w x))])
                         (format "(def ~a\n  (~a ~a))\n" (first t) (second t) (third t))))]
         [doc   ((make-rope buf) text)]
         [table (make-hash)])
    (define (frame n m)
      (define stats (make-hash))
      (define segs  ((segment/memo (line-window/s n m inner)
                                   #:key (covering-key) #:table table #:stats stats) doc))
      (define plain ((segment ((line-window n m) inner)) doc))
      (define (view segs) (map (lambda (s) (cons (car s) (format "~a" (cdr s)))) segs))
      (printf "  window ~a..~a:  ~a pieces   hits ~a  misses ~a  table ~a   agrees-with-plain ~a~n"
              (~a n #:min-width 2) (~a m #:min-width 2) (~a (length segs) #:min-width 2)
              (hash-ref stats 'hit 0) (hash-ref stats 'miss 0) (hash-count table)
              (if (equal? (view segs) (view plain)) 'yes 'NO)))
    (frame 0 7)      ; first paint
    (frame 4 11)     ; scroll down 4: lines 4..7 already cached under the CANONICAL key
    (frame 4 11)     ; repaint: everything in view hits
    (frame 2 9))     ; half-leaf offsets: covered leaves hit, straddled ones recompute

  (printf "~n========== viewport: ((line-window 1 1) lines*lisp) ==========~n")
  (show both-bundle ((line-window 1 1) line*lisp)
        "(define (f x) ; twice\n  (g \"a\nb\" x))")
  (printf "~n========== viewport: ((line-window 1 2) lisp) on 4 lines ==========~n")
  (show both-bundle ((line-window 1 2) lisp-run-guide)
        "(a b)\n(c \"s\" d)\n(e)\n(f g)"))
