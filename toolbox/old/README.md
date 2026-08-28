# toolbox/old

**Prior-generation optic snapshots** — the superseded `algebra.rkt` helper
libraries, kept as pre-replacement references (the optic *core* only; the cursor
machines they backed are not snapshot here). Each is self-contained — no requires;
`raco test` runs it standalone. Live successor: `toolbox/stage.rkt` (the staged /
church-store optic), with `toolbox/algebra.rkt` still the live store-shaped one.

- `algebra-store-shaped.rkt` — the **store-shaped** generation: the optic as two
  pure fields, `opt (peek view)` — the peek the forward half of the iso
  `w ≅ (put · foci)` (get/set derived projections, `opt-update` a derived command),
  the pure-viewer channel under its monoid; `spl` (section + retraction) with `iso`
  its substruct. The immediately prior generation to the staged optic.
- `algebra-record-opt.rkt` — the **record-opt** generation before it: the optic as
  a plain three-field record, `opt (get set f)` — the accessors as the ops, a stored
  transform `f`, `attach-viewer` rendering over the *view*.

Older generations, cold, in `deprecated/`: `deprecated-7` (van Laarhoven optics),
`deprecated-1..6` before them.
