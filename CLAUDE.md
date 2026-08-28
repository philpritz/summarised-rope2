# summarised-rope2

An experimental successor core to `../summarised-rope` (the "old repo"): editing
structured text over a persistent summarised rope through the **segmenting
split** — classifier guides that cut a rope into tagged pieces — rather than the
zipper. All new work so far is draft code under `scratch/`; the stable files are
copies from the old repo.

## Orient before working — read in order, in full

1. **`discussions/conventions.md`** — how sessions are run (the working
   agreement). Imported below, so it is always in context. Its own "Session
   start" list was written for the old repo — where the two differ, THIS list
   wins (see the deviations note).
2. **The latest discussion notes** — under `discussions/`, newest date folder,
   highest number first. `discussions/2026-07-21/1-claude.md` is the founding
   design record: the segmenting split, its guide algebra, and the open threads.
3. **The scratch files** — `scratch/segment.rkt`, `scratch/render.rkt`,
   `scratch/mini-edit.rkt`: the current work, all draft, exercised by demos in
   their `module+ main`.
4. **The copied sources, as needed** — `rope-core.rkt`, `summaries/`, `toolbox/`:
   the old repo's tested code, verbatim except the flagged `module+ internal`
   provides added in `summaries/lisp-summary.rkt` and
   `summaries/sexp-summary.rkt`.

## Deviations from the conventions' session-start list

- The conventions' project line still describes the old repo (rope + zipper);
  this project's subject is the segmenting split. The zipper may yet enter, but
  nothing here uses it.
- There is no `design-notes/nomenclature.md` here yet; the old repo's copy at
  `../summarised-rope/design-notes/nomenclature.md` still serves for vocabulary.
- **No git repository yet**: skip the git-log orientation step, and the
  push / branch / merge conventions are inoperative until `git init`. Until
  then the discussion notes are the only durable session record, so notes carry
  detail the routine-changes-live-in-the-commits rule would normally leave to
  the log.
- Old-repo file references inside the conventions (reference example notes such
  as `discussions/2026-06-19/2-claude.md`) resolve only in the old repo.

@discussions/conventions.md
