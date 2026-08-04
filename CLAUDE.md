# CLAUDE.md

## What this is

`plumb` — an experiment in shell pipelines built from threads and channels
carrying Lisp objects, instead of processes and byte streams. SBCL only
(`sb-thread`, `sb-mop`), no external dependencies, no Quicklisp/ocicl needed.

```
sbcl --eval '(asdf:test-system "plumb")'   ; 137 assertions, all passing
make                                       ; dump bin/plumb
sbcl --script demo.lisp
```

## Design decisions already made — don't relitigate these without reason

- **A stage contains no concurrency.** Ports arrive via the dynamic variables
  `*input*` and `*outputs*`, bound by `spawn-stage`. A stage body is an ordinary
  loop. All coordination lives in `send`/`recv` and one `unwind-protect`.
- **Two independent close flags per channel.** `producer-closed` is EOF and
  travels forward; `consumer-closed` is SIGPIPE and travels backward. They are
  not two views of one state.
- **`recv` returns two values** so `nil` remains a legal payload.
- **`recv` drains the buffer before reporting EOF.** Checking `producer-closed`
  first loses in-flight objects when a fast source finishes early.
- **`send` re-checks `consumer-closed` after every wakeup**, not just on entry.
  A producer already parked on a full channel is precisely the case `take`
  creates; entry-only checking deadlocks it.
- **`close-input` drops the buffer.** Up to `capacity` objects of arbitrary size
  would otherwise stay reachable from a channel nobody will read again.
- **Backpressure is the only flow control.** No laziness in the data
  representation; `*default-capacity*` is the single knob.
- **Errors are condition objects**, sent out the `:err` port and recorded on the
  pipeline, not formatted to text.
- **`close-output` is refcounted.** The `:err` port is shared by every stage, so
  the first stage to finish must not close it for the rest; `run` sets
  `channel-producers` to the stage count. Without this a *live* reader on `:err`
  saw EOF before the error arrived, every time.
- **`present` owns the one-object-one-line rule.** A stage runs in its own
  thread and does not inherit the caller's `*print-pretty*`, so each printer
  used to rediscover this separately — and drift. Give a type a `present`
  method rather than special-casing it at a print site.
- **Type checking happens before any thread is spawned** (`check-pipeline`).

## Invariants to preserve

1. `spawn-stage`'s cleanup must run on all three exit paths — EOF, `(finish)`,
   and error — or teardown stops cascading and pipelines hang.
2. `(finish)` closes the input channel on the way out; the upstream `send` then
   signals `channel-closed`, which the handler treats as *normal* termination.
   Breaking this turns `(list (counter) (take 5))` into an infinite loop.
3. Every test is wrapped in `sb-ext:with-timeout`. Bugs here present as hangs,
   not backtraces — keep new tests time-boxed.

## Open work, roughly in priority order

1. **Stage fusion.** `where`, `xform`, `take` and other simple transducers should
   collapse into one thread. A thread per stage is fine at 6 stages; it is not
   fine when a loop spawns a pipeline per file. Needs a `fusable-p` flag on
   `stage` and a pass in `run` that composes thunks.
2. **Fan-out / `tee`.** Pipelines are linear today. `stage-ports` already exists
   but `run` only wires `:out` and `:err`. Needs a graph builder *and* a
   decision on copy-on-fanout: objects crossing a channel are shared references,
   so two branches mutating one row is a bug class real pipes cannot have.
3. **External processes, the rest of it.** `sh` and `to-sh` (`src/process.lisp`)
   cover the source and sink shapes: lifetime is handled in `with-command`'s
   `unwind-protect`, and a non-zero exit signals `command-failed`, which rides
   the existing `:err` port. Two pieces remain.
   - *A mid-pipeline filter* (`objects -> stdin`, `stdout -> objects`). It must
     write and read the child concurrently or the pipe buffers deadlock, and it
     also blocks in `recv`, which is a condition variable and so cannot be
     selected on alongside file descriptors. That forces a helper thread inside
     the stage, i.e. an explicit exception to "a stage contains no concurrency".
     Decide that deliberately or not at all.
   - *A PTY path* for interactive programs. `sb-ext:run-program` takes `:pty`,
     which is the hook; nothing uses it.
4. **Presentation, the rest of it.** `present` (`src/present.lisp`) is the
   generic, and `table` renders aligned columns driven by `fields`. What is
   still missing is object identity retained per screen region
   (CLIM-presentation style), so previous output stays live and clickable.
5. **Reader, the rest of it.** `src/reader.lisp` implements word mode: `|`,
   `{...}` blocks, `.field`, earmuffed variables, `(...)`/`#'f` escapes, and
   the leading-paren dispatch. Still missing: `1kb`-style suffix literals,
   globbing, and redirection. These decisions are settled -- don't re-open
   them while adding to it:
   - `{...}` emits `($ ...)` and `.name` emits `(fld :name)`. The reader is a
     source-to-source pass; the result is handed to the existing `eval`, so
     stages, `present`, teardown and `help` all work unchanged on day one.
   - **Prefix inside braces** — `{(> .size 1kb)}`, not `{.size > 1kb}`. Infix
     would mean inventing precedence and associativity; prefix means the braces
     hold ordinary Lisp and every function you already have works untouched.
   - **A block is always an argument, never a stage.** Write
     `ls | where {(> .size 1kb)}`; `ls | {(> .size 1kb)}` is not shorthand.
     A bare block after `|` reads equally as `where` or `xform`, and picking
     wrong turns a filter into a mapper silently. One rule — stage name, then
     arguments — beats a special case.
   - `{}` is the word-mode/expression-mode boundary, which is its real job.
     Splitting on `|` therefore has to track brace depth and string literals,
     since both can contain a `|`.
   - **A leading `(` means Lisp, anything else means word mode.** No flag. The
     rule applies at every level, not just the start of a line: a `|` segment
     beginning with `(` is a Lisp form used as a stage, and an argument
     beginning with `(` is a Lisp form evaluated in place. That is the general
     escape hatch — whatever the word syntax cannot say, parentheses can.
   - **Earmuffs are variable syntax.** A bare word spelled `*x*` or `+x+` is a
     variable reference; every other bare word is a string. Without this,
     auto-detect regresses `plumb '*default-capacity*'`, which prints 64 today.
     The rule is syntactic on purpose: a lookup-based one ("is this word
     bound?") would mean a new `defvar` silently changes how an existing script
     parses. Globs survive it, since earmuffs need `*` at both ends with
     content between — `*.lisp` and `*` stay strings. A file genuinely named
     `*size*` needs quoting.
   - **`.name` bare is shorthand for `{.name}`** — `sort-by .size` is
     `(sort-by ($ (fld :size)))`. Braces are only needed when the block does
     more than read one field.
   - **A trailing keyword means T** — `sort-by .size :desc` is `:desc t`.
     Shell flags do not carry values.
   - **`sh` stays explicit.** No falling back to an external command when the
     first word is not a known stage. That fallback is what shells do, but it
     is a lookup rule, and under it a mistyped stage name silently becomes a
     failed exec instead of an error.

## Conventions

- `defstage` declares `(:consumes X) (:produces X)` right after the docstring.
- `emit` and `finish` are macros — never `#'emit`.
- `do-input` declares its variable `ignorable`; don't add `(declare (ignore ...))`
  inside the body, it isn't a valid declaration position there.
- New stages go in `src/stages.lisp`, new exports in `src/package.lisp`.
