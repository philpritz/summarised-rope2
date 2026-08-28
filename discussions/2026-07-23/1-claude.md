# 2026-07-23 / 1 — the state×mode×wrapper editor; intrinsic rope metadata; the framed-guide algebra and memoized segmentation

A **mixed session**: an implementation arc rebuilding the editor
(`scratch/mini-edit2.rkt`), two small landed extensions to the foundation
(`rope-core.rkt`, `toolbox/memoize.rkt`), and a large design arc — resuming the
founding note's memoization thread — that ended in a second landed scratch
module, `scratch/segment2.rkt`: the guide algebra over callable framed structs,
with segmentation memoized through the toolbox memoizer. Everything new is
scratch; nothing is promoted.

## The editor restructure — `scratch/mini-edit2.rkt`

A fresh file beside `mini-edit.rkt` (which stands as the working reference).
The editor splits in two:

- **State** — `(struct st (doc cursor vis hl prev mode wrapper keymap))`: the
  document, every guide pointing into it, and the key-interpretation machinery.
  The mode is *stored*, not derived from the cursor's spec as before; the trade
  recorded: storing loses the old single-source-of-truth (cursor spec ⇒ mode)
  and gains modes as first-class values changeable in-editor — the user's
  motive. Cursor/mode agreement is maintained by routing every switch through
  the mode's `enter`.
- **Mode** — only what genuinely differs, church-encoded:
  `(enter h l j k select keep)`. Essentially the old tuple; `select` (né
  `target`) produces the before/focus/after region `x`/`r` act on.
- **Wrapper** — `wrap : mode -> keymap`; every generic command (insert,
  delete, replace, backspace, visual, search, undo, mode switch, `dd`) defined
  once against the mode's slots. Stored in the struct alongside its product,
  the cached keymap, so switching rebuilds via `(wrapper new-mode)`.
  - Keymap fork: (a) fn over the whole command string — rejected (opaque, no
    enumeration); (b) key→parameterized-transform map — the enumerable option;
    (c) fn over parsed tokens. **Chosen: (c)** per the user: commands are
    read-tokenized to symbols/numbers, counts split (`3 l`, `2 dd`), and the
    keymap is a variadic fn pattern-matching the token list. After `i` / `r` /
    `/` the rest of the line is one string argument (Racket `read` wrapped;
    `\n` escape kept for inserts).
- **Undo**: `prev` holds the whole pre-edit state; the chain *is* the history
  (a popped state's own `prev` is the rest). Movements don't push. Consequence
  accepted: undo restores mode, selection, and highlight too.
- **Visual selection generalized**: both ends of a selection are cut guides,
  so charwise-grid and structural-sexp selection collapse into one `'cut`
  kind; only linewise `V` stays special (row-inclusive coordinates). `dd` is
  its degenerate case — the line region at the cursor through the same
  `region-delete`.
- **At-cursor edits** (insert/backspace) re-find the cursor with the plain
  char flip (`flip-guide`): the edit leaves the back name unchanged, so the
  per-mode flips (`flip-linecol`/`flip-sexp`) aren't needed on this path.
- Dropped: the sexp-mode `X`-as-form-delete alias (`X` = backspace uniformly).
- **Fix found by the user**: sexp `j` at an atom minted an unreachable spine
  (`(0 0 0)` inside `define`), and `l` past the end drifted — movements were
  blind spine arithmetic. Movements now normalize through the doc: arithmetic,
  then the mode's own `enter` lands the candidate and reads the real index
  back, so unreal addresses never enter the state. (The movement slot already
  received the doc; sexp had ignored it.)

## The document is a rope

- `st` holds a rope, not a string (user: everything should be a rope — the
  string round-trip rebuilt the rope per keystroke, discarding structure
  sharing and any hope of memo reuse). Edits keep the reassembled rope;
  untouched pieces share structure.
- String helpers became summary reads (`line-count`, `line-len` via
  `line-end-at`, `offset->pos` by landing `char-at`); `trim-op` cuts a rope;
  the editor's bundle gained `occur-smr` (lazy per word — free until `/`) and
  `hash-smr` (no-op-edit check by fingerprint).
- `render-viewport` accepts a rope as-is, reading side-factor summaries
  through `rope-algebra`; strings still work (its own demo path).

## Landed foundation extensions (both flagged as new vs the old repo's copies)

- **`rope-core.rkt`**: every node carries two intrinsic fields beside
  leaves/height — `rope-length` (chars) and `rope-hash` (Karp–Rabin
  fingerprint; `kr`, a private transplant of hash-smr's monoid, since the
  dependency points the other way). Motive (user's): memo keys must not
  depend on what rides the bundle — `char-smr`/`hash-smr` aren't guaranteed.
  Shape-blind, so content identity survives rebalancing. Tests added.
- **`toolbox/memoize.rkt`**: `memo-on!`/`memo-off!` — off passes every call
  straight through, entries kept warm (orthogonal to `memo-clear!`); and `#f`
  from `#:key` is now a reserved answer — "don't cache this call". Tests
  added (37 pass).

## Design arc: memoizing the segmenting split

Resumes the founding note's fork (b) — an equivalence on (rope, guide) pairs
computed by canonicalization. The session narrowed it to the concrete case
(one obscuring line view, depth-one nesting, per the user) and re-derived the
machinery on the toolbox memoizer instead of `segment/memo`.

- **The requirement that shaped the key** (user's): the view must never enter
  the key — different viewports covering a subtree, and the bare inner guide
  itself, must produce the *same* key. So a covered focus keys by the
  **inner** guide; straddling/out-of-view/tiny foci answer `#f` (no entry).
- **Contexts stay summary values.** Where decided: the founding guide contract
  (`(bs fs as)` over summary values) — no fork was recorded there.
  Tree-shaped contexts were explored this session: possible (smr coercion
  already makes guides summary-or-rope agnostic), would make keys fully
  intrinsic and share structure with the doc, needs a raw non-fusing
  `ctx-join` exposed from rope-core. **Parked** — the user chose to keep
  summaries + `hash-smr` fingerprints for now.
- **Signature fork** (how to make the walk's step cacheable — the step must
  receive `(bs t as g)` somehow): (a) open step `cut : bs t as g`; (b) open
  recursion + a `memoize-fix`; (c) contexts riding a value ("framed");
  middleware hooks, memoizing the guide itself, and path-based keys were
  rejected (entanglement; caches answers not walks; edit-locality lost).
  Closure-framing (rope-core's `frame`) was ruled out for caching — fresh
  opaque closures per step, unreadable context, the "never frame-and-store"
  trap. Transparent-struct framing is information-equal to (a)/(c), so the
  choice fell to what it does to the guide algebra.
- **Chosen: all guides are framed** — one struct, `(struct guide (smr bs as
  fn))`, the frame *is* the guide (user's call, resolving the
  bare-vs-framed-door risk by uniformity: there is no unframed kind to pass,
  and `make-guide` always seeds empty contexts, so context enters only
  through the walk's folds or an explicit `frame`).
  - The **smr is the guide's lens** (user's design): it seeds the empties,
    accumulates context on descent, and projects every value the fn sees —
    classifier fns are written in their own summary domain, replacing the
    normalize-through-your-own-smr boilerplate.
  - **Callable, two arities** (user's call): `(g fs)` field-mode — the walk's
    call, contexts from the fields; `(g bs fs as)` argument-mode — a
    composite calling its factors with its own projected values, each factor
    re-lensing.
  - **`frame` re-derived**: outer fold onto the fields (`guide-left`/`right`
    are the inner folds — four one-sided folds, freely composing by
    associativity). `(segment t (frame g b a))` is the deliberate
    fragment-in-context entrance; demoed.
- **The view: `(struct viewport (n m inner))`** — callable like a guide,
  contexts delegated to `inner`, interrogable by the key. Naming fork:
  `obscure` (too vague), `blind`/`window` considered; `mask` avoided (the
  founding note's *rejected* fork vocabulary); **`viewport` chosen** — no
  conflicts (`render-viewport` is a distinct identifier; comments already use
  the word for exactly this). Represented as its own struct rather than a
  spec field on `guide`; the spec-field variant is the recorded alternative,
  whose pull is product transparency (the cursor-as-degenerate-viewport
  rewrite needs to see inside products) — parked with it.
- **The key** (`viewport-key`): focus by `rope-hash`/`rope-length`
  (intrinsic); guide slot `(smr . fn)` — the parts stable under the walk's
  struct-copies (the struct itself is freshly copied every step and would
  never repeat); contexts via `ctx-key` — a bundle context enters by its
  projected `hash-smr` component, a *narrow* context by its raw value.
  Found live: `hash-smr` cannot be applied to a foreign summary value
  (single-arg application still folds), and the raw-value fallback is sound —
  **self-keying**: a narrow guide reads nothing its context value doesn't
  carry, so equal values imply equal answers; narrow guides thereby get
  exact, non-probabilistic context keys — a corner of the projections
  upgrade arriving early.
- **Lens limitation, documented**: bundle projection is atomic-or-identity
  (a bundle value projects components or passes through its own bundle;
  sub-bundles fail), so multi-summary guides (`lisp-run-guide`, the classic
  lifts, `keywordize`) take the working bundle as their lens; single-summary
  guides carry atomic lenses. Sub-bundle projection in `summaries.rkt` is
  the recorded upgrade path to truly minimal lenses.

## The landing — `scratch/segment2.rkt`

- The two structs, the folds, `line-span`, `viewport-key`, and `segment` as a
  factory whose *body* is worn by `memoize` (recursion through the memoized
  identity — an entry per level; `splice` welds the child seam). The factory
  returns the memo itself — `memo-on!`/`off!`/`memo-size` apply directly;
  plain segmentation is the same body with a refusing key.
- The old classifier bodies run **verbatim** inside the new structs (internal
  projections become pass-throughs), so `lisp-run-guide`, `occur-guide`,
  `keywordize`, `guide->sides`, `guides->region` are one-line wraps of
  `segment.rkt`'s functions; `line-guide` and `cursor-at` are rewritten
  new-style (boilerplate-free) as the showcase. Conversely the structs answer
  `(g bs fs as)`, so the **old `segment` runs them unchanged** — the referee
  for the entire demo battery.
- Demo battery (24 cross-checks, all passing): oversized viewports and the
  bare guide sharing one entry set (one root hit each after the first paint);
  cold scroll down/up with pure-hit returns; the off/on switch; one-row
  scrolling on an unaligned 64-line document holding at 3–6 misses per frame
  (boundary-shaped cost, entries accumulating); narrow-lens self-keying;
  the framed fragment reproducing the whole document's pieces; keywordize ×
  cursor × region agreeing with the old walk on the same struct.
- Also verified along the way (session scratchpad experiment, superseded by
  the in-file battery): a windowless whole-document paint warms every
  subsequent windowed frame — cache population is shared in both directions.

## Status

- All landed work is **scratch or flagged foundation additions**; nothing
  promoted. Test state: rope-core + summaries batteries pass with the new
  fields (≈1,900 checks), memoize 37, segment2's 24 demo cross-checks, both
  editors' demo scripts run clean.
- `mini-edit2.rkt` runs on the **old** algebra (`segment.rkt`), which stands
  untouched.
- **Open threads**: migrate the editor onto `segment2` (wire the memoized
  segmenter into render/edit paths — the rope-holding state was the
  prerequisite); the supersession questions (`viewport` vs `line-window`, the
  new cache vs `segment/memo`+`covering-key`, struct interrogation vs
  `guide/s` — and `mini-edit2` vs `mini-edit`); sub-bundle projection in
  summaries; cursor-as-degenerate-viewport (wants product transparency — see
  the spec-field alternative); tree-shaped contexts (parked); straddle
  relativization (still parked from the founding note); `segment2`'s
  `#:cap`/eviction untested under pressure.
