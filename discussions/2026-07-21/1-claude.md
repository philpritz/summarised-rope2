# 2026-07-21 / 1 — the segmenting split, its guide algebra, and an editor loop over it

A **mixed session**, and the founding session of `summarised-rope2`: mostly design /
exploration — the segmenting split and the algebra of guides over it — with everything
implemented along the way landing as **scratch** (`scratch/`). Nothing is promoted;
every `.rkt` written this session is a draft artifact.

## Repo bootstrap

- `summarised-rope2` seeded from the old repo (`../summarised-rope`, "the old repo"
  below): `toolbox/` (including its `compiled/` and `old/` — undecided whether to
  drop), `rope-core.rkt`, `discussions/conventions.md`.
- Later copied verbatim as needed: `summaries/{sexp-summary,summaries,lisp-summary,
  summary-laws}.rkt`. Two copies gained a `module+ internal` provide block (flagged
  in comments at the site): `lisp-summary.rkt` exports the arm plumbing
  (`(struct-out arm)`, `aref`, `mode->i`, `boundary?`), `sexp-summary.rkt` exports
  `sexp-head`/`sexp-tail`. These are scratch-enabling only, not public surface.
- Conventions edited this session: the ADR-shape section removed (forks are recorded
  inline again, per *Record the alternatives*).
- No git repo initialised yet; no CLAUDE.md / README yet. Organisation of the new
  repo beyond `scratch/` + copies is still open.

## The segmenting split (the session's core design)

- **A (segmenting) guide is a classifier** `(bs fs as) -> tag | #f` over summary
  values: `bs`/`as` the accumulated left/right context, `fs` the focus.
  - tag → the focus is homogeneous: one piece, labelled.
  - `#f` → the focus straddles a boundary: descend into its halves (a leaf splits as
    the simulated branch, halving its text — rope-core's existing convention).
  - **Totality obligation**: a guide must tag every indivisible (single-char) piece;
    that bounds the descent.
- `(segment g) : rope -> (listof (cons tag subrope))` — one right-to-left walk;
  adjacent `equal?`-tagged pieces **weld** (rope-join). Cost ∝ boundaries × log,
  not leaves: a subtree inside one segment is approved at its root, never opened.
- Relation to `multisect`: multisect locates cuts named by guides (count fixed up
  front); segment *discovers* the cuts and *labels* the pieces (count is data).
- Relation to the old experimental `guide*`/`multisect*` (rope-core experimental
  submodule): segment covers the same ground with one return value instead of three
  booleans, plus tags; welding replaces the `fuse?` logic; the `inside?` gap-mode
  adjacency test is subsumed by tag identity. **Open (parked): whether segment
  supersedes `guide*` (delete the submodules) or both live.** Tentative read: strict
  improvement.
- **Guides normalize their inputs through their own smr** (`(linecol-smr bs)` etc.),
  so one guide runs over its plain summary or any bundle containing it. This is the
  load-bearing composition convention of the whole session.
- **`guide-product`**: tag = list of factor tags; classify iff every factor does;
  welding on tag equality is componentwise. Segmentation by a product = the
  **coarsest common refinement** of the factors' segmentations. If each factor is
  total on single chars, so is the product.

## The worked guides

- **line-guide** (over `linecol-smr`): single-line = no newline, or exactly one as
  final char — *a newline belongs to the line it ends*, forced by the totality
  obligation (a lone `"\n"` must tag). Tag = line number = `lines(bs)`.
  - A trailing empty line yields no segment (segments partition characters); the
    empty-line-with-terminator case IS represented. Consumer (editor) draws the
    phantom last line; not the segmentation's concern.
- **lisp-run-guide** (over `lisp-smr`; needs `char-smr` in the bundle): tags
  - `(code spine)` — one token (atom / bracket) + its trailing ws; spine = floored
    front spine at the piece's left edge. Every token step is a spine step, so
    parens differentiate structurally (rainbow-by-depth reads off spine length).
  - `(string spine)` / `(charlit spine)` — whole lexical constructs; in the emitted
    domain they ARE atoms (the `~` placeholder), so they carry slot + depth.
    Spine rounding: a piece starting AT its construct rounds the head **up** to the
    atom's slot (it leans before the not-yet-begun `~`); a piece starting INSIDE
    rounds **down** (the atom has begun). Charlits always round up (their `~` is
    emitted only at the payload char).
  - `(comment idx)` / `(block idx)` — emitted whitespace, NO sexp presence; `idx` =
    run index at the left edge (boundaries strictly left + the one this piece's
    class opens), which keeps `;a\n;b` apart. *The spine subsumed the run index
    everywhere a construct has sexp presence* — the run index survives only here.
  - Code homogeneity = equal floored spines at the focus's two edges, **guarded by
    the fragment's emitted head class** (`sexp-head` ∉ {open, close}): a complete
    balanced form + trailing ws floors both edges onto the form's own slot while the
    spine dips inside — found live as a render bug (`(fact n)␣␣` painted as one
    bracket). A code token starting with a bracket can only be the lone bracket.
  - Single-char escape hatch: a 1-char focus passes unconditionally (a lone bracket
    *is* a spine step) — this is why `char-smr` must ride the bundle.
- Known cosmetic quirk: an **unterminated** string splits `"` from its interior
  rather than welding (the never-completed `~` shifts the rounding at the quote).
  Not load-bearing; noted, unfixed.

## Views: guide transformers, the coagulation equation

- A **view is just a function guide → guide**; pass-through IS calling through. No
  protocol, no reserved answers. `(line-window n m)` answers `'before`/`'after` for
  out-of-view foci (they weld into one blob per tag), `#f` at edges (descend), and
  calls the wrapped guide in view. Out-of-view subtrees are approved at their roots
  — cost confined to the viewport + O(depth) at its edges. Tags in view stay
  **absolute** (the wrapped guide sees the true contexts).
  - Fork, three rounds: (1) `obscure` with a mask protocol wrapping out-tags in
    `(hidden tag)` — *rejected*: the machinery has no business authoring tag
    vocabulary; (2) mask tags verbatim but with reserved `'in`/`#f` answers —
    *rejected*: `'in` is a pointless reserved word when delegation can just be a
    call. (3) plain transformer — chosen.
- **The coagulation equation** (the algebraic spec for views): for a view expressible as a
  pure tag function `v` (possible because tags carry what views read — the line
  factor), `segment (W v g) = (coagulate v) ∘ (segment g)`, where `coagulate` maps
  `v` over tags and re-welds. The fused wrapper is the *optimization*; the
  equation's conditions: **blanket soundness** (only v-monochrome foci get blanketed) and the
  **hereditary guard** principle — a wrapper may prune exactly when its pass-through
  guard is monotone under taking subropes (line spans shrink ⇒ line-window
  qualifies). Pruning is possible precisely because v-monochromality is
  summary-decidable at coarse foci even where g's own homogeneity isn't.
  - Consequence adopted as a rule (restated 2026-08-01, see undernote): **a
    view's extension is tag-determined; its summary reads only prune.** The
    wrapper itself is necessarily a full `(bs fs as)` classifier — blanketing
    answers at coarse foci, where the inner guide still says `#f` and no tag
    exists to read — but it must compute `coagulate v` of the inner
    segmentation for some pure tag function `v`; summaries decide only *where*
    it may answer early, never *what* the answer is beyond `v`'s reach. The
    constraint on tag vocabulary design is intact: a view can only obscure by
    factors the tags carry — that is exactly the demand that `v` exist.
  - The equation is rackcheck-able (segment-wrapped vs coagulate-of-plain); not
    yet written as a test.

## Memoization of segmentation

- Motivation: scrolling — re-cutting a viewport each frame should reuse subtree
  segmentations. Problem: the view fused into the guide makes the window invisible
  context (same subrope + contexts, different window → different answer); keying on
  `(n, m)` kills reuse across scroll.
- **Fork: how to key the cache.**
  - (a) Memoize the *unwrapped* side via the coagulation equation (cache fine seglists,
    apply `coagulate` per frame) — workable, but view-specific machinery.
  - (b) **Chosen: an equivalence on `(rope . guide)` pairs, computed by
    canonicalization** — generalizes (a): equations like coagulation become
    rewrite rules producing the canonical representative. Chosen because it scales with the
    combinator vocabulary (each new wrapper ships a rewrite + its hereditary-guard
    proof) instead of demanding a bespoke cache story per view.
- Pieces (all in `scratch/segment.rkt`):
  - **`guide/s`** — transparent guides: `(spec impl)`, callable via
    `prop:procedure`. Contract: **spec equality must imply extensional equality**
    (impls pure functions of their spec). Opaque guides degrade to eq-keyed.
    `line-window/s` builds the spec'd viewport.
  - **`segment/memo`** — deliberately dumb memoizer: at each focus asks
    `key : (bs fs as spec) -> key | #f`; `#f` = don't memoize (ALL policy lives in
    the key fn: canonicalization, context identity, refusals); otherwise
    lookup-or-fill the subtree seglist, spliced with seam welding.
  - **`covering-key`** — the default: strip covering `line-window`s (span from
    linecol summaries ⊆ window → the wrapper vanishes, recursively), key the
    canonical tuple by `hash-smr` fingerprints of (bs, fs, as) + canonical spec;
    `#f` on straddle/outside and tiny foci. Full-content `hash-smr`, NOT the
    ws-blind tokhash — whitespace matters here.
  - Demo result (4-leaf doc, leaf-aligned windows): first paint 30 misses; scroll
    +4 lines → 1 hit (the shared leaf, at its topmost covered node) + misses for
    new text only; repaint → all hits, 0 misses; misaligned window → straddled
    leaves descend but their cached *sub*-entries hit (memo at every level buys
    partial reuse). Every frame cross-checked equal to the unmemoized run.
- Decided: `bs`/`as` enter by **fingerprint** for now; the declared upgrade path is
  **projections** — key on the guide's *observation* of the context (lisp guide:
  entry arm + frontier of bs, first-class of as; line guide: `lines(bs)`), so edits
  invalidate only entries whose observations changed. (This is the absolute-vs-
  relative-tags tension in another coat; **relative tags themselves: parked** — a
  real redesign, wait until an edit-heavy use forces it.)
- Parked: the **straddle relativization** rewrite (window → focus-local overlap
  coords, letting border subtrees share too). Strictly additive.
- Caveats standing: fp keys make correctness probabilistic (~2^-61; a collision
  serves a wrong seglist); table unbounded (cap with `toolbox/lru.rkt` when it
  matters); `covering-key` does not yet look inside product specs (a cursor factor
  in the product defeats it — the cursor-as-degenerate-window rewrite is the known
  fix, unwritten).

## Keywords

- Goal: keyword highlighting (operator-position only) without scanning text.
  Operator position is already in the tag: spine `(0 k ...)`, depth ≥ 1.
- **`tokhash-smr`**: Karp–Rabin as a monoid (summaries' `hash-smr` design) with one
  contrivance — *whitespace chars contribute the identity*, so `"define \n"` hashes
  as `"define"`. Safe in this context because a code piece is one token + ws by
  construction. `keyword-table`: precomputed hash → keyword.
- **Fork, two rounds:**
  - (a) Post-weld retag pass (group consecutive pieces sharing an operator spine =
    one token, hash the group — monoidal tokhash combines across splits) —
    *superseded*: it's a phase after segmentation (kw not intrinsic in tags), and
    its ws-grouping bled keyword paint onto a following line's indent.
  - (b) **Chosen: boundary-atom hashes + a guide wrapper.** `atomhash-smr`: a
    boundary monoid whose value carries the token hash (and, since the refinement
    below, the LENGTH) of the fragment's first and last atom runs, an
    all-one-run? flag, and the head char class. Any piece reconstructs its WHOLE
    atom as `trail(bs) ++ own ++ lead(as)` — three cached reads, O(1) joins.
    `keywordize table : guide -> guide` retags operator-position `(code spine)` to
    `(kw spine)` when the whole-atom hash hits; head-class `'delim` (ws-only
    pieces) never retag — kills the bleed. kw is intrinsic in the tag; the
    renderer just paints. A cursor parked mid-keyword keeps the highlight (each
    half reconstructs the same whole atom), and an edit that de-keywords the atom
    un-paints it with no extra machinery.
  - Deviation from the proposal as spoken (hashes as a field *inside* `lisp-smr`):
    housed as a **bundle component** instead — same reads, zero surgery on the mode
    machine, consistent with the bundle composition story. Cost: atom boundaries
    are char-class-based (ws, brackets, `"`, `;` are delimiters; `#` `|` are not),
    so pathological glue adjacency (`a#|c|#define`) misreads — false negatives
    only. The exact version (fields in `lisp-smr`'s arms, mode-gated) is the
    upgrade path if it ever bites.
- Aside from the hash discussion (recorded for reuse): no small sketch can decide
  arbitrary substring queries even probabilistically (INDEX lower bound ⇒ Ω(n));
  what exists: exact-per-position (KR subrange hashes, O(log n) on the rope),
  exact-per-known-word (the old `occur-summary` — word-agnostic value, trie cache),
  probabilistic prefilter (k-gram Bloom as a boundary monoid — no false negatives,
  linear size, prune-then-verify). Also sketched (chat only, not in scratch): the
  **hashed smr wrapper** — value = (fp, interned lazy content), content memoized by
  fp, shape-blindness making the memo survive rebalancing where the old
  cell-identity caches go cold.

## Cursors, navigation, flips

- **`guide->sides smr g`** — lifts any classic guide `(L R) -> -1|0|1` into a side
  factor (`'before`/`'after`/descend) by judging the focus's two edge cuts.
  `cursor-at p` (char gap) is the special case. Product with a side factor forces
  the segmentation to cut exactly at the cursor, even mid-atom.
  - Tie-break added later: a target strictly inside ONE char's emission (a quote
    emits gap + atom start in one char, so its flush cut doesn't exist in the
    text) resolves to `'after` — land at the char's start. Without it the slot
    guide refuses an indivisible piece (found live: navigating to a string).
- **Grid**: `linecol-at` (row-then-col lexicographic), `line-end-at`, and
  `leftmost-guide` (Kleene min) — `grid-at r c = leftmost(linecol-at r c,
  line-end-at r)`: the editor clamp (column past the line end lands at the line
  end) IS the min, no conditionals. Re-derived from the old `lisp-view.rkt`.
- **Flips** (all: current guide + current text → renamed guide; locate the cut once
  with `multisect`, rename it off the right context):
  - `flip-guide` — char back name (k chars from the end).
  - `flip-linecol` — grid back name (rows from end, chars to the line's newline —
    both read off ONE linecol value of the right part). Survives whole-line
    insertions above, which shift every front (row . col).
  - `flip-sexp` — back HEAD (floored to the flush slot name — a mid-atom cut reads
    B+½, found as a landing bug), front PATH (shared by both anchorings, after the
    old `base-right`), offset flipped from since-start to to-end. **Structure-true**:
    survives tail edits that change char counts but not form counts, where the
    char/linecol flips drift (demo: `cc` → `cccc`; sexp flip holds at char 4, char
    flip drifts to 6).
  - Editing discipline that falls out: edits LEFT of the cursor anchor through a
    flip (insertion advances past the new text, deletion holds, a deleted newline
    lands at the join — no cursor arithmetic anywhere); edits RIGHT of the cursor
    (vim `x`) need no flip at all — the front name is already stable there.
- **Sexp navigation**: `spine-cmp` (outermost-first lexicographic; each component
  picks its side by sign — back < -1/2; prefix precedes extension = a cut deeper
  than the target sits right of it; toolbox `lexicographic`), over
  `lisp-spines`. Movement is arithmetic on the index: sibling = ±1 on the head,
  out = cdr, in = `(cons 0 ix)`; the end-slot clamp comes free from the comparison
  geometry.
- **The refined index `(spine . offset)`** — mid-atom positions as first-class sexp
  targets (the classical spine names slot boundaries only; the old repo represents
  mid-atom cuts as ±½ heads but deliberately excludes them as *targets*).
  `atomhash` gained `llen`/`tlen` (lengths beside the hashes): `tlen(L)` at a cut =
  chars since the atom's start, `llen(R)` = chars to its end. `lisp-slot-guide+ ix
  k`: the spine decides, except for cuts inside the target slot's atom (head =
  spine head + ½, same outer path), where the offset decides — front family
  compares `tlen(L)`, back family `llen(R)`. `nav-index` reads a cut back as a
  refined index (fractional heads round by cut kind: leading ws names the NEXT
  slot / ceiling, mid-atom the CONTAINING one / floor + tlen offset).
  - Caveat: offsets inside string atoms inherit atomhash's char-class blindness
    (quotes/inner ws are delimiters to it); exact for plain atoms. Same upgrade
    path as keywords.

## Edits on the seglist

- **`backspace`** (segment.rkt): operate ON the tagged list — the cursor is in the
  tags, so its offset is the char-sum of the `'before` pieces (cached reads); trim
  the last before-piece, hand the pieces back to `(make-rope smr)` (coerces,
  re-fuses the seam, rebalances; untouched pieces share structure), recut. A
  deletion that changes lexical state (killing a quote) relabels everything
  downstream — the recut derives truth, it doesn't patch.
- The recut is currently FULL per edit; routing edits through `segment/memo` (with
  context projections so upstream-edit invalidation is bounded) is the known next
  step, designed but not wired.

## The display + editor (the integration proof)

- **`scratch/render.rkt`** — `render-viewport text n m [cursor]`: segment with
  lines × keywordized-lisp (× side factor) under `line-window`; the TAGS drive
  everything: line factor → gutter; classes → colors; bracket depth = spine length
  (opener len-1, closer len-2); `before`/`after` blobs → "(N lines hidden)" with
  the count off the blob's own linecol; cursor = reverse video on the first char of
  the first `'after` piece (phantom block at line ends / doc end). Cursor accepts a
  char offset, a `(row . col)` pair, or a classic guide (side factor via
  `guide->sides` over the bundle). 256-color "wine-dark" palette (user's).
- **`scratch/mini-edit.rkt`** — line-command editor over the whole stack:
  `[n]h/l/j/k`, `0`/`$`, `[n]x` (delete-at, front-anchored), `[n]X`/`[n]b`
  (backspace via flip), `[n]dd`, `i <text>` (`\n` escape), `q`; `s` toggles **sexp
  mode**: hjkl become structural (refined-index round-trip: `pos->rix` →
  `spine-move` → land with `lisp-slot-guide+` → read row/col back), edits anchor
  through `flip-sexp`, the status line shows `spine+offset`, rendering cuts at the
  refined target. Goal-column behaviour falls out of clamping in the landing
  rather than the state. Windows console note: `read-line` needs `'any` mode or
  commands arrive as `"j\r"`.
- One design point from the editor: sexp mode stores plain (row . col) between
  commands and re-derives the refined index per operation — position state stays
  coordinate-agnostic; only the anchor/interpretation changes per mode.

## Status

- **Everything from this session is scratch** (`scratch/segment.rkt`,
  `scratch/render.rkt`, `scratch/mini-edit.rkt`), exercised by demos in their
  `module+ main` (segment.rkt's main covers: lines, lisp, product, keywordize,
  cursor, flips incl. the sexp-flip drift demo, refined index, memo hit counts,
  viewport) — no rackunit battery, no law tests yet. The copies under `summaries/`
  and `toolbox/` are the old repo's tested code, unmodified except the flagged
  `module+ internal` blocks.
- Not ready to promote; the shape of promotion (which pieces become `text-edit/`-
  style layers, what the public surface is) is untouched.
- **Open threads, gathered**: guide*/multisect* supersession; segment/memo wired
  into edits + the cursor canon rule; context projections; relative tags (parked);
  straddle relativization (parked); coagulation-equation rackcheck test; lru cap on the
  memo table; exact (arm-housed) atom boundaries for keywords + string offsets;
  unterminated-string weld quirk; repo organisation (README/CLAUDE.md, whether
  toolbox/old and compiled/ stay).

## Undernotes — amendments from later sessions

- **2026-07-23 (second session that day): the framed algebra's calling
  convention is one arity.** In `scratch/segment2.rkt` — the framed-guide
  algebra of `discussions/2026-07-23/1-claude.md`, where guides are structs
  carrying their own `bs`/`as` — the two-arity callable (field-mode `(g fs)`
  plus argument-mode `(g bs fs as)`) collapsed to field-mode only: context
  enters a guide solely through its fields, so the uniformity that chose the
  framed struct ("context enters only through the walk's folds or an explicit
  `frame`") now holds literally. A caller with context in hand frames first,
  `((frame g b a) fs)`; `as-old` packages exactly that as an old-style
  classifier, which is how this note's `segment` still runs the structs as
  referee. The contract recorded HERE — `(bs fs as) -> tag | #f` — is
  untouched for `segment.rkt`'s guides, and survives inside the structs as the
  classifier fn's lens-projected internal signature; only the external calling
  convention changed. Cost accepted: `guide-product` frames each factor per
  judged focus (a struct-copy, of the kind the walk already does per step).
- **2026-07-23 (same session): welding generalized to `#:merge`.** `segment`
  (in `segment.rkt`) now takes an optional merge fn — a *partial semigroup on
  tags*: `(merge a b)` yields the welded piece's tag, `#f` refuses. The
  equal-tags weld recorded here is the default (`(and (equal? a b) a)`), so
  existing callers are unchanged. New obligation on any supplied merge:
  associativity, definedness included, else the seglist would depend on tree
  shape. The driving case: style span runs carrying their grid start — tag =
  `((row . col) . style)`, merge welds on style equality with the left start
  winning — which recovers line/column information *without* line cuts (a
  span welding across newlines still knows where it starts; verified in a
  scratchpad demo, same 43 spans with positions as without). Rhyme noted:
  this re-admits the generality of `guide*`'s `fuse?` (which welding-on-
  identity had subsumed, above), but principled — merge reads tags only.
  Alternative kept open: positions accumulated *after* segmentation by
  folding `linecol-smr` over the seglist — memo-friendlier (start-bearing
  tags are maximally absolute, the relative-tags tension above at its
  sharpest), but positions invisible to views. Neither ruled out.
- **2026-08-01: the "views read tags, not summaries" rule restated —
  extension, not implementation.** As originally recorded, the rule's bald
  statement contradicted what a blanketing wrapper *is*. To answer
  `'before`/`'after` at a coarse out-of-view focus — where the inner guide
  still says `#f`, so there is no tag to read — the wrapper must be a full
  `(bs fs as)` classifier reading summaries. A view lifted as a pure tag
  function (`v ∘ g`, restyle-style) answers only where `g` answers, so the
  walk descends to `g`'s full fineness before there is anything to transform:
  the labels coarsen, the cut does not, and none of the window's descent
  savings materialize. The rule is therefore restated as a constraint on the
  wrapper's **extension**, not its implementation: the wrapper must compute
  `coagulate v` of the inner segmentation for some pure tag function `v` —
  that `v`'s existence IS the coagulation equation, and what licenses
  `viewport-key`'s strip — while its summary reads may only *prune the
  descent* (the guard deciding where an early answer is allowed, disciplined
  by the hereditary-guard monotonicity), never make it a function tags could
  not express. The rule's body bullet is reworded accordingly. Its tag-
  vocabulary force survives verbatim: a view can only obscure by factors the
  tags carry.
