# Discussion File Conventions

## Session start

**Project.** A persistent Racket rope that caches a user-defined summary at every node, with an s-expression zipper for structural navigation and editing.

**To orient: read in order.**

1. This file, in full.
2. `design-notes/nomenclature.md` — the project's working vocabulary.
3. The latest discussions.
4. Recent `git log` — skim the last several commits; the messages record what landed
   (refactors, surface changes) and live in git, not in the notes.
5. The source files.

---

This folder keeps compact notes from design sessions.

## Naming

Discussion files should be named so chronological order is visible from the
path, even when different assistants or collaborators are involved.

Use a date folder, then put the chronological number before the source:

```text
discussions/YYYY-MM-DD/N-<source>.md
```

where:

- `YYYY-MM-DD` is the session date.
- `N` is the chronological session number for that date.
- `<source>` is the main collaborator/source, for example `claude` or
  `chatgpt`.

Examples:

```text
discussions/2026-05-27/1-claude.md
discussions/2026-05-27/2-claude.md
discussions/2026-05-27/3-claude.md
discussions/2026-05-27/4-chatgpt.md
```

If an older file is missing a number or uses a flat filename, prefer moving it
into the date folder when convenient, rather than leaving chronology implicit.

## Collaboration style

Keep project discussions tight and user-led:

- Let the user propose ideas first.
- Do not go off designing or thinking ahead independently.
- Avoid fluff.
- Keep responses compact unless more detail is requested.

## Proposals are exploratory by default

When the user proposes an idea, it is opening a discussion, not a go-ahead to
implement. The default is to explore it together and get a feel for it:

- Discuss the shape, trade-offs, and alternatives first.
- Trying code is welcome, but show it inline in chat as a sketch. Do not write
  it into project files, commit, or push.
- Stay in this exploratory mode, iterating, until the user explicitly signs off
  on adding it.
- Only after sign-off: make the change in the project, then commit/push per the
  rules below.

A proposal is an invitation to think together, not a task to rush to done.

What "let's have X" means in different contexts:
- **Exploring:** wishful thinking, not an instruction — *suppose we had X* /
  *suppose it were so*; conjure the design and talk about it.
- **Mid-landing** (push mode, editing a file): it inherits the file
  destination, per *Amendments inherit the last destination*.

## Questions are questions, not instructions

When the user asks a question, answer the question. Do not infer a plan, a
design decision, or a go-ahead from it.

- A question like "do we need X?" or "is Y used anywhere?" is a request for
  information, not a request to remove X or change Y. Answer what was asked and
  surface the findings. (If instead the question probes a choice in your own
  work — say code you drafted in chat — feel free to revise it if you wish.)
- **Above all, do not close off design avenues on your own.** In these
  discussions the user decides which possibilities to rule out; your job is to
  keep them open — lay out options and trade-offs, don't prune them.
- When unsure whether a question is also a request to act, ask before acting.

This pairs with the exploratory-by-default rule above: both keep the work from
racing ahead of the user's lead.

## "Draft" means draft in chat

When the user asks to *draft* something, produce it inline in chat — not in a
project file. Drafting is collaborative: show the text, revise it together, and
write it into a file (conventions, notes, code) only after the user signs off.

- "draft X" / "show X" → show it in chat first; iterate until sign-off, then
  write.
- A direct instruction to add concrete content already in front of you ("put
  this in …", "add this") is a go-ahead to write it straight away — no draft
  round needed.
- Sign-off must be explicit and affirmative ("write it", "add it"). A request to
  *change* the draft is a new revision round, not approval — after applying the
  edit, re-show the result and wait. When in doubt, stay in chat.
- A tentative "maybe X" depends on state: while still drafting in chat it means
  keep revising (never a write); once the content is already in the file, apply
  it by default unless you think of a serious objection, in which case we'll
  discuss. If the "maybe" carries a question ("…what do you think?"), that's
  soliciting an opinion (see *Questions are questions*), don't apply.
- "Draft" means in chat.
- But a request to **push** (or otherwise land it) alongside the draft overrides the
  chat-first default: write it and push, no separate sign-off round.
- Land work — write to a file, commit, or push — only when the user's own words
  ask for it ("write up", "add it", "push"), or confirm an assistant's offer with
  "go ahead". A revision request or a hook prompt is never sign-off.

So drafting is the collaborative mode; an explicit "add this" is not.

## When to record

Do not be eager to write discussion notes. Recording happens when the user
decides to finish the session, not mid-thread. Keep designing until then;
write the note only when the user calls the session done.

## Routine changes live in the commits — since 2026-06-24

From 2026-06-24, an implementation or routine change with no major decision or fork
needs no dedicated session note — the commit(s) are its record (orientation skims
`git log`). Write a note only when a session decided a **fork** worth keeping (with
its losing options, per *Record the alternatives*) or did design / exploration whose
thinking the diff cannot carry. Sessions before this date wrote a note regardless,
so the older `discussions/` are denser than this rule would now produce — expected,
not an oversight.

## Two kinds of session

Sessions fall into two categories; name the category in the note's opening line
(existing notes already do: "An **implementation session**: …"). Their notes
follow different guidelines:

- **Implementation sessions** — the work lands in files: code, renames, surface
  changes. The diff is the record, so the note is a short **bulleted list** (per
  *Content style*): what landed, the test state, and only the forks that were
  genuinely decided along the way (with their losing options, per *Record the
  alternatives*). No narration of the work itself. Reference examples:
  `discussions/2026-06-19/2-claude.md` (rope-core rewrite) and `3-claude.md`
  (zipper-core pare-down).
- **Design / exploration sessions** — the work is the thinking: options weighed,
  shapes tried in chat, directions parked or rejected. Little or nothing lands,
  so the note is the only artifact — give the ideas, the alternatives, and the
  reasons full space, per *Record the alternatives* and *Space is proportional
  to the fork*.

A mixed session splits: its design arcs at design weight, its landings at
implementation weight.

## Content style

Keep notes compact and durable:

- Record decisions and open questions, not full transcripts.
- **Notes are bulleted by default.** A note is a list — one bullet per decision,
  fork, or open thread; a prose paragraph is the *exception*, used only for
  genuinely connected narrative. Distinct decisions scan as a list, not as prose
  that runs them together. This holds even when a short summary is asked for: a
  short note is a *few bullets*, not a dense paragraph.
- **Draft to the prescribed shape, and flag conflicts.** Before drafting a note,
  restate its required shape from this file (category line; bulleted; forks with
  their losing options; parked/draft marked) and draft to that. If the request
  asks for a conflicting form — "a paragraph," "a short blurb" — surface the
  conflict and confirm before drafting, rather than silently following the literal
  form. (A requested form is a premise to verify; see *Verify the user's
  premises*.)
- Mark parked ideas explicitly as parked.
- Mark draft code clearly as draft code.
- Include small code snippets only when they capture the shape of the design.
- Mention the concrete files affected when useful.

## Record the alternatives, not only the choice

A decision is half-recorded if the note says what was chosen but not what it was
chosen *over*. The reasoning — the options weighed, why each was set aside, why
the survivor won — is the most valuable and least recoverable part of a session;
capture it alongside the decision, not as an afterthought.

For each decision that mattered:

- list the **options considered**, including ones that were attractive and only
  narrowly lost;
- for each one set aside, give the **specific reason** it lost — the concrete
  cost or flaw, not just "we preferred the other";
- say why the **chosen** option won — what it buys, and what it gives up.

Distinguish **parked** (a live option merely deferred) from **rejected**
(considered and ruled out, with a reason); both belong in the note, only their
status differs. A third, opposite disposition: **dropped** — a choice adopted
then reversed, or whose premise later vanished. Unlike parked and rejected, it
and its reasons need not be recorded.

One exception: **purely intermediate representations are not alternatives.** A
draft stage the winning design merely passed through within the session is
scaffolding, not a fork — it was never weighed *against* the final shape, only
refined *into* it, and it references nothing a reader can resolve. It stays out
of the note; see *Record only the final shape* under *Write for a reader who
wasn't there*.

This is what lets a later session build on the work instead of re-deriving it:
settled questions stay settled because the rationale is right there, and a wrong
turn is cheap to undo — the runners-up and their reasons are already written, so
you resume from the fork, not from scratch. Keep it proportionate, though: a
decision turned over once needs a line or two per option, not a transcript. The
test is whether someone arriving at the fork cold could see why it went the way
it did.

## Record the reason as a dependency

A decision is only as settled as the reason that drove it, so record the grounds,
not just the choice — especially a reason the user gives, which is the real
dependency. Without it written down, a choice calcifies: it looks settled long
after its support has vanished.

**Prefer the user's stated reason, and the one they state first.** They usually
mark it — "if …", "since …", "so that …", "only … if …"; that clause *is* the
dependency, recorded in their terms. When several are given, the initial / most
emphatic one is load-bearing and must be captured; don't swap in a tidier reason
of your own, even if yours is also true. (A "maybe" or "I think" may hedge the
decision's feasibility, its downstream consequences, or the reason itself — use
discretion to decide between them, and record accordingly.)

**Don't fabricate reasons.** A reason supplied where none was given reads to the
next reader as a real constraint, falsely stiffening the design and narrowing
where it can still move. Record only reasons actually given or genuinely
operative; leave a choice unattributed rather than invent grounds.

## Space is proportional to the fork, not the work

A note's length tracks the weight of the ideas decided, not the hours spent. Long-term
directions, major forks, and their losing alternatives are what a later session cannot
recover from the repo — give them the space. Low-level decisions — implementation
shapes, argument orders, naming, code style — are recoverable by reading the files, so
they get proportionally less: anything from a passing gloss ("a style pass over
rope-core") to a line or two of colour, as feels right. There is no duty to itemise
routine work — the diff is its own record.

- The test: at each line, would a reader need the note to understand the choice, or
  would the code answer them? Spend the note's space on what only the note can say.

## Write for a reader who wasn't there

A discussion note is a **standalone artifact** — decipherable from the repo alone,
without the conversation that produced it. The transcript evaporates; the note is
what survives, so nothing in it may depend on the transcript to be understood.

- **Design the examples — don't transcribe the chat's.** Examples raised live are
  chosen for the back-and-forth, not the page: ad-hoc, half-stated, tangled with
  context, and rarely the clearest teaching case. You are positively *encouraged*
  to rework them or invent fresh ones — pick whatever conveys the idea most cleanly
  and elegantly. Fidelity is to the *idea*, not to the example that happened to
  come up; a cleaner illustration you build yourself is the better record.
- **Build on the repo, not the conversation.** Leaning on an earlier dated note or
  the code is fine — they're durable and a reader can follow the pointer. Leaning
  on what "we just said" is not.
- **Record only the final shape.** Refinements over intermediate constructions
  that were considered purely within the session should not appear in the note:
  they reference nothing in the code — neither after the session nor before it —
  and nothing in the previous notes, so a reader has no way to resolve them. This
  bounds *Record the alternatives*: an alternative worth recording is a design
  weighed **against the final shape**, described in place with the reason it lost.
  The session's own draft stages of the winner are not alternatives — they are
  scaffolding, and they drop out with the session.

## Refine a draft with agents

When a request to write or draft a note mentions **agents** — "write the draft and
refine it with agents", "spawn agents to see if the ideas get across" — don't just
write it: write it, then *test it on fresh readers and revise from what they miss*.
The reader is a future session, exactly the audience; this is the runnable form of
*Write for a reader who wasn't there*.

The loop:

- **Spawn a fresh agent** — one with no access to the current conversation; that's
  the point. Hand it the draft, the core files it describes, and any prior notes it
  points to. Read-only.
- **Probe answer-free.** Ask it to reconstruct the decision and answer specific
  questions, but never reveal the answers in the prompt. Require it to **attribute**
  each answer to the *note*, the *code*, or its own *inference* — that split
  separates "the note conveyed it" from "a capable reader filled it in."
- **Read its report** (it returns to you, not the user); the gaps are wherever it
  leaned on the code or inference, or flagged something asserted, unclear, or
  over-claimed.
- **Revise, then re-test with a new fresh agent** — a clean slate each round, no
  carryover. Two or three rounds usually converge; stop when a fresh reader clears
  it end-to-end.
- Keep the revisions in the working tree until sign-off (the draft rule holds).

## Status sections

When a discussion produces code, include a short status section that says what
is complete, what is not complete, and whether the work is ready to promote or
only a draft artifact.

## Merging to master

Two distinct actions, not to be conflated:

- **Push the feature branch.** Committing and pushing to the session's feature
  branch is routine — it just syncs the branch to the remote. Plain "push" /
  "upload to GitHub" requests mean this, and it is *not* a merge to master.
- **Merge to master.** Only when the request explicitly contains the word
  "merge" or "master" — e.g. "merge to master", "push to master", "merge this".
  Plain "push" / "upload" never triggers it; without one of those words, the
  feature branch is never folded into `master`.

## Amendments inherit the last destination

A follow-up amendment is handled the same way as the most recent comparable
action, without the user restating how. The disposition changes only when the
user redirects.

- If the last change was **pushed to master**, subsequent amendments are also
  committed and pushed to master — no need to re-ask.
- If the last change was **pushed to a feature branch**, subsequent amendments
  go to that same branch.
- If the current mode is **drafting in chat**, follow-up tweaks are likewise
  drafted in chat (still subject to explicit sign-off before they land).

This refines *Merging to master*: the word "merge"/"master" is still needed to
first send work to master, but amendments to the same work then stay there by
default.

## Verify the user's premises, hedged or assumed

When the user supplies a premise — hedged ("unless I'm mistaken," "if I'm
right," "check me") or carried in a conditional ("if nothing else calls this
…") — check it before acting on it or building on it:

- **Premise holds** → proceed, and confirm they were right.
- **Premise is wrong** → do *not* proceed on it. Point out what they
  overlooked, so the mistaken step never lands.

A confident assumption with no hedge word is still a premise to check, not a
fact to take on faith. When the premise carries an instruction, that
instruction is a go-ahead conditioned on it; when it's only stated, confirm or
correct it before relying on it.

Unlike a bare question (see *Questions are questions*), a premise is something
to verify and act on — just conditioned on its being true.

## One branch per session

A session's pushed work goes on a single feature branch by default, not a fresh
branch per sub-task. The session's first push creates it
(`claude/<date>/<topic>`); everything after lands on that same branch —
including changes that feel separate, like a conventions tweak made alongside
code.

Don't open a second branch for a side change mid-session. If a change genuinely
belongs on its own branch, the user will say so; the default is together.

Pairs with *Amendments inherit the last destination*.

## Where a new convention goes

- **Prefer an existing section.** Slot a new convention into the section that
  already covers its topic; add a new section only when none is appropriate.
- **Keep it to a bullet.** A small amendment is a bullet, not a section --
  reserve new sections for genuinely new, substantial topics. This keeps the
  file's structure stable and its rules findable, rather than accreting thin
  one-rule sections.

## When a convention is ambiguous

Feel free to ask when it's genuinely unclear which convention applies, or how to
apply it.
