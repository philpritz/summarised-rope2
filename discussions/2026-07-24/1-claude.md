# 2026-07-24 / 1 — span-run display: cut semantic, join by style; explicit algebra arguments; the editor's display-memoization design; resumable seglists (parked: the guide/frame split)

A **mixed session**: an implementation arc landing the span-run display pipeline
(`scratch/restyle.rkt`, new; `scratch/mini-edit2.rkt` rewired onto it; `#:merge`
in both segment walks; explicit-algebra signatures), and a design arc on how the
editor's display should meet the memoized segmenter — ending in the resumable-
seglist design (not landed) and a core redesign of `segment2`'s guide struct
(**parked**). Two of the landed changes are also recorded as undernotes at the
end of the founding note (`discussions/2026-07-21/1-claude.md`), a section added
this session for amendments to recorded decisions.

## Landed: the span-run display — `scratch/restyle.rkt` (new)

The architecture, in the user's phrase: **cut at the semantic joints, join at
the syntax ones.** Segmentation cuts as finely as meaning requires; the theme
re-welds whatever it cannot tell apart; display output is *span runs* —
`(style . text)` pairs, the format most highlighting systems converge on —
falling straight out of `segment` with no fused renderer in between.

- **`restyle`** — `v : semantic tag -> style tag` lifted over a guide (a pure
  tag function, so the founding note's coagulation equation applies; the
  equation is checked in the demo: fused `restyle` = `coagulate` over the
  semantic seglist).
- **`with-head`** — `(code spine)` → `(code head spine)`: the emitted head
  class (open/close/…) read off `lisp-smr` the same way `lisp-run-guide` reads
  it internally. Exists because the old renderer's `paint` peeked at a piece's
  first *char* for bracket dispatch — a text read a pure tag map cannot make,
  so the head class moves into the tag. Completes the *tags carry what views
  read* rule (restated 2026-08-01 in the founding note's undernotes).
- **`face`** — the wine-dark theme as a tag function; the style tag IS the SGR
  code list.
- **`with-start` + `span-merge`** — positions in the tags: tag =
  `((row . col) . payload)`, the grid start read off bs's linecol; welding by
  `span-merge` = "weld when everything-but-position agrees, keep the left
  start" — deliberately generic over the payload, so the SAME merge serves
  the semantic weld inside `segment` and the style weld inside `coagulate`.
- **`coagulate`** — map `v` over a seglist's tags and re-weld: the unfused
  form of `restyle`, now sharing `segment`'s `#:merge` vocabulary and taking
  the rope fn explicitly (below). **The chosen display pipeline** (fork
  below): `segment (with-start g) #:merge span-merge` cuts once, semantically,
  positions in the tags; `coagulate rope (over-pos face) #:merge span-merge`
  applies the theme downstream. Verified bit-identical to the fused path.
- **`render-spans`** — the stateless renderer: every span self-locates (a
  span at col 0 opens its line; interior newlines open row+i; a final empty
  chunk belongs to the next span), so the old renderer's walk state
  (`at-start`, line counting) becomes data reads. A span could be rendered
  without reading its predecessors.

**The display-pipeline fork** (three rounds, each superseding the last —
recorded because the loser is instructive):

1. *Fused only* (`restyle` inside `segment`) — works, law-checked; kept as
   the law's other side. Lost as the primary: styling and positions bake into
   the segmentation's tags, so every theme/frame variation re-segments.
2. *Everything post-pass* (positions accumulated by folding linecol over the
   seglist) — memo-friendliest; kept open in the founding note's undernote.
   Lost for the editor: positions invisible to anything upstream.
3. **Chosen: segment keeps `(row . col)`; the theme is downstream.** One
   positioned semantic seglist is THE shared artifact; `coagulate` with the
   same `span-merge` derives display per theme. User's call.

## Landed: `#:merge` in both walks; the founding note's undernotes

- `segment.rkt`'s `segment` takes `#:merge` — welding generalized to a
  **partial semigroup on tags** (`(merge a b) -> tag | #f`), default = the old
  equal-tags weld. Obligation: associative, definedness included, else the
  seglist would depend on tree shape. Undernoted in the founding note (with
  the `fuse?` history rhyme and the position-tension alternative).
- `segment2.rkt` collapsed to **one calling arity** `(g fs)` — context enters
  a guide only through its fields via `frame`; `as-old` bridges to old-style
  `(bs fs as)` classifiers (how the old segment still referees the battery).
  Also undernoted.
- The undernotes section itself is new: amendments from later sessions,
  appended to the note whose decisions they amend.

## Landed: explicit algebra arguments

User preference driving it: *algebras should be given explicitly, not derived
from the data* (`rope-algebra` remains the wear-transparency channel, used
where nothing else is possible). The division that emerged — argument type
tracks what the function does with it:

- **Join-only consumers take the rope fn** (a `make-rope` factory — the
  factory IS the join): `coagulate`, and `segment2`'s walk (`segment rope
  #:key #:cap`, `split`, `splice` — `rope-algebra` no longer appears in the
  file). Notable: the framed-guide design made segment2's walk summary-free
  (all summary work lives in the guides), which is what makes rope-fn-only
  coherent there — itself evidence for the framed design.
- **Summary-computing machinery takes the smr**: `segment.rkt`'s old segment
  applies the algebra as an algebra (context seeds, focus summaries, context
  joins) and cannot recover it from a factory — it stays smr-shaped, with
  rope-core's `multisect`/`frame` as the explicit-smr precedent.

## Landed: the editor's display rewired — `scratch/mini-edit2.rkt`

`show` now runs the chosen pipeline: one positioned semantic segmentation (no
line factor — line structure rides the tags' grid starts), `coagulate` through
`face*`, `render-spans`. `render.rkt` is out of the editor (stands as the
fused reference implementation, used by its own demo and `mini-edit.rkt`).

- **The cursor became a piece** — `cursor-block-at p`: a one-char factor
  tagging `'before`/`'after`/`'block`; the block is a tag `face*` maps to
  reverse video, replacing the old renderer's armed-flag-and-substring slice.
- `face*` absorbs the backgrounds (selection over search, block over both) —
  they were always tag-derivable.
- Phantom blocks stay positional, by nature (the cursor gap has no glyph): a
  block ON a newline renders as a reversed space before the break; the
  doc-end block is a one-line postscript.
- Edit paths (`cut-at`, `subst-focus`, `occur-edit`) unchanged on the old
  segment; the display viewport never constrains edits.

## Design: the editor × the memoized segmenter (not landed)

The problem, named by the user: **partially memoize only the syntax-in-the-
viewport information, stable as cursor and selection change.** Scrolling is
already solved — `viewport-key`'s covering-strip makes cached entries
window-independent (a covered focus answers identically under any covering
window). Cursor and selection are the hard part, because their influence on
the output is real. The layering that resolves it — cache by rate of change:

| layer | changes when | where it lives |
|---|---|---|
| syntax + positions | the text changes | the memo (per-subtree seglists) |
| window | scrolling | the wrapper; the key strips it |
| cursor / selection | every keystroke | seams only + downstream retag |
| theme / faces | rarely | `coagulate`, downstream of everything |

- **The design crime**: letting a fast layer's information into a slow
  layer's cached values. Tagging pieces `'before`/`'after`-of-cursor does
  exactly that — the marks flip document-wide per keystroke, aging the whole
  cache. Hence: the walk must not tag by cursor at all; side/block/focus
  marks are POSITION-DERIVABLE downstream from the tags' grid starts.
- **The cursor's true cache footprint is its seams** — `p` and `p+1` (and a
  selection's two edges) must be piece boundaries; everything else is
  identical to the cursor-free segmentation. Cost per keystroke = the spines
  over the old seam (healing) and the new one — O(depth), the same
  boundary-shaped cost as scrolling. One uniform law: boundaries cost,
  interiors are free.
- The parked cursor-as-degenerate-viewport thread resolves conceptually: a
  cursor is not a degenerate *window* (a window hides; the cursor hides
  nothing) — it is a degenerate **seam-forcer**. The viewport was two
  mechanisms all along: obscuring (strippable) and its edge cuts
  (unstrippable, boundary-priced).
- **Fork: where the seams come from.** (a) A `view` struct carrying
  `(n m cuts inner)` — the key strips it when covered AND no cut falls
  strictly inside the focus; cursor geometry enters the walk's key. Lost to:
  (b) **resumable seglists** (below) — the walk stays entirely cursor-free,
  the key untouched; seams are cut *after* segmentation, list-locally, in the
  piece(s) containing the offsets. (b) is also cheaper: piece-sized, not
  depth-sized.
- Migration plan (standing, unimplemented): `segment2` gains `#:merge`
  (its `splice` still welds on `equal?` — positioned tags never weld without
  it); struct wrappers for `with-head`/`with-start` (with-start INSIDE the
  viewport — anything wrapped around it blinds `viewport-key`'s strip);
  `st` gains a scroll row; **the display guide and segmenter must be
  persistent values** — the memo keys the guide slot by `(smr . fn)`, so a
  product rebuilt per frame is a fresh key and zero hits (rebuild only when
  `hl` changes); blob markers in the renderer (count read off the blob's
  linecol — the bare blob tag carries none); edit paths stay on the old
  segment.

## Design: resumable seglists (user's proposal; not landed)

A piece becomes a struct carrying `(tag, frame, rope)` — the guide *as framed
at that piece's position*, exactly the `(t, g)` pair the memoized step
operates on. A piece is a **suspended segmentation**; the seglist becomes a
closed calculus of operators: coagulate (join), obscure (blobify), and
**refine** (re-cut — the user's staging: first segment by syntax + viewport,
then cut the result by selection and cursor).

- Refinement by offsets **inherits the parent tag verbatim** rather than
  re-judging: homogeneity is hereditary for the current guides but is not a
  law of guides, so inheritance is the safe semantics. The carried frame is
  for the other refinement — resuming with a finer guide.
- Quiet bonus: **lazy deepening** — a hidden-region blob carrying its
  suspended walk can be segmented later, in correct context, without
  re-walking the document (scroll toward it and refine; same shape for
  refining with occur when `/` is pressed).
- Welding resumable pieces needs the carried frames merged: the joined
  piece's frame = left's bs, right's as — a field projection, no summary
  math. Fork on the operation's shape: (i) `merge-guide` over two framed
  guide structs (struct-copy the as across; recurse through viewport
  wrappers) — worked but rejected by the user: an operation on two guide
  structs *suggests they could be different guides, which they cannot be*;
  (ii) welding sites re-curry from contexts in scope — only available inside
  the walk; (iii) → superseded by the guide/frame split, parked below.

## PARKED: the guide/frame split

The user's redesign of `segment2`'s core, left for a later session. Guides
lose their context fields — `(struct guide (smr fn))`, the *constant* of a
walk — and contexts live in a separate transparent applicable record, the
curried application: `(frame g bs as)`, callable on `fs`. Driving reason (the
user's): merge-guide's two-guide signature misrepresents the data — there is
ONE guide and two positions; welding should *construct* `(frame g bs₁ as₂)`,
not merge. Consequences worked out in discussion, recorded for resumption:

- The carried thing must be the curried application made **transparent** (an
  applicable record, not a bare closure): a closure's contexts are unreadable,
  so the memo key, welding, and resumption (which must extend contexts) all
  fail on it — the same rock as the founding "never frame-and-store" trap.
- The walk threads one frame; the folds stop recursing through viewport
  wrappers (the viewport no longer contains contexts).
- The memo key improves: the guide slot is `g` itself, `eq?`-stable across
  the walk, replacing the `(smr . fn)` extraction.
- Products recover the cost the one-arity change introduced: factors are
  context-free values called with the composite's projected arguments — no
  per-factor-per-call framing.
- Argument order noted but not settled: codebase convention says
  `(g bs as) -> fs -> tag`.

## Status

- All landed work is **scratch**; every battery passes: `segment.rkt` demo,
  `render.rkt` demo, `segment2.rkt`'s 24 cross-checks (through `as-old`),
  `restyle.rkt`'s demo (coagulation equation HOLDS; positioned pipeline agrees with
  the fused path; content round-trips), `mini-edit2.rkt`'s full script
  (cursor block, occur backgrounds, selection across lines, doc-end phantom,
  undo all verified in the frames).
- **Supersession questions widened**: `render.rkt` (fused) vs the
  `restyle.rkt` pipeline; `mini-edit` vs `mini-edit2`; plus the standing
  segment2-vs-segment.rkt set.
- **Open threads**: the editor × memo migration (designed above, nothing
  landed — includes `#:merge` for segment2's splice); resumable seglists
  (designed, nothing landed); the guide/frame split (**parked**);
  sub-bundle projection; context projections (bounded edit invalidation);
  tree-shaped contexts (parked); straddle relativization (parked);
  `#:cap`/eviction untested under pressure.
