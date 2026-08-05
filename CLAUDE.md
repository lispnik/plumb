# CLAUDE.md

## What this is

`plumb` — an experiment in shell pipelines built from threads and channels
carrying Lisp objects, instead of processes and byte streams. SBCL only
(`sb-thread`, `sb-mop`), no external dependencies, no Quicklisp/ocicl needed.

```
sbcl --eval '(asdf:test-system "plumb")'   ; 382 assertions, all passing
make                                       ; dump bin/plumb
sbcl --script demo.lisp
```

## Design decisions already made — don't relitigate these without reason

- **A stage contains no concurrency**, with one deliberate exception: `tee`
  calls `run` to start its branches. It stays within the rule where it matters
  -- all coordination is still `send`/`recv` and one `unwind-protect`, and the
  threads belong to `run`.
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
- **A shared stream needs `with-output-lock`.** A CL stream is not thread-safe
  and fan-out lets several stages print at once; without it two branches
  duplicate and drop each other's lines, differently on every run. Per line for
  `print-items`/`peek`, around the whole render for `table`.
- **Metadata comes from one `lstat`, never from opening the file.** `open` needs
  read permission and blocks forever on a FIFO; `ls` used to hang on a
  directory containing one. `lstat` also describes a symlink rather than
  following it, which is right because `glob` does not resolve them.
- **A hand-written alien struct layout must be cross-checked.** `src/stat.lisp`
  declares Darwin's `struct stat` to reach nanoseconds, `st_blocks` and
  `st_birthtime`. Every field `sb-posix` also knows is asserted equal to it in
  the suite, so a wrong offset fails on a known value instead of quietly
  returning plausible nonsense in the fields nothing else can check.
- **Filenames are strings, not pathnames.** `glob` reads them with `readdir` and
  matches them itself; CL pathname patterns hold `*` as a pattern object, so
  `file-namestring` re-escapes it and `lstat` then fails -- which silently
  dropped every file whose name contained a metacharacter. Build pathnames
  only at the end, with `parse-native-namestring`.
- **The glob matcher parses, then matches.** Alternation, closure and negation
  must be able to give up a choice and retry, which a character walk cannot do.
  Patterns are parsed once per component, not once per filename.
- **Extended glob syntax is unconditional**, unlike zsh's `setopt extendedglob`.
  A file literally named `foo~bar` or `a^b` therefore needs escaping; that was
  a deliberate choice, not an oversight.
- **Type checking happens before any thread is spawned** (`check-pipeline`).
  `T` on the consuming side means any object *type*, not the absence of one, so
  nothing may follow a stage that produces `nil`. Reading it the other way let
  a sink follow a sink, and `explain` reported that pipeline as fine.

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
2. **Fan-out, the rest of it.** `tee` (`src/stages.lisp`) fans one stream into
   several pipelines, built on `run :input`. Copy-on-fanout is settled:
   objects are **shared**, and copying is a stage (`(xform #'copy-file-entry)`
   at a branch head) rather than a flag -- nothing can deep-copy an arbitrary
   Lisp object correctly. Sharing means a mutating branch is a data *race*,
   since `tee` sends to branches and emits onward concurrently.
   The general graph is done too: `run :ports` wires the extra ports a stage
   declares to their own branches (`wire-branches`), and `route` is the worked
   example. Ports carry their own types -- `:produces` describes `:out` alone
   -- and routing stages use `try-emit`, since `emit` is strict on purpose.
   Port names are one flat namespace per `run`; deeper graphs nest by putting a
   routing stage inside a branch. What is left is cosmetic: `explain` can only
   draw the ports it is handed, so `explain foo | route ...` in word mode has
   no way to name branches yet.
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
   the leading-paren dispatch, `1kb`-style suffix literals, and `>` `>>` `<`
   redirection. Globbing needed no reader syntax: `*.lisp` is already a string,
   and `ls` globs it (see `glob` in stages.lisp). These decisions are settled
   -- don't re-open them while adding to it:
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
   - **Suffix literals are a substitution, not a reader macro.** Catching
     `1kb` at read time means owning the digit characters and reimplementing
     CL number syntax -- floats, ratios, radix, and the symbols `1+` and `1-`.
     Two narrow rewrites instead: on the token in word mode, and on the symbol
     `|1KB|` inside a block, which is read as ordinary Lisp. Sizes are binary
     and durations are seconds; minutes are `min` because `m` is megabytes.
     Known wart: the block rewrite does not respect `quote`.
   - **A bare `.name` collides with dotfiles**, and cannot be disambiguated:
     `.gitignore` is lexically identical to `.size`. Quoting is the escape, and
     `glob` says so rather than reporting a type error about a lambda. The
     alternative is dropping the bare shorthand and always writing `{.size}`,
     which trades a common convenience for a rare one. A lone `.` is below the
     two-character threshold, so `ls .` is a path.
   - **Reserved words wrap the whole pipeline.** `explain ls | take 5` reads as
     `(explain (list (ls) (take 5)))`, the way bash's `time` does. This is a
     closed list (`+reserved-words+`), not a general prefix mechanism -- the
     latter would be the stage-position guessing ruled out above.
   - **`sh` stays explicit.** No falling back to an external command when the
     first word is not a known stage. That fallback is what shells do, but it
     is a lookup rule, and under it a mistyped stage name silently becomes a
     failed exec instead of an error.

## Conventions

- `defstage` declares `(:consumes X) (:produces X)` right after the docstring,
  and `(:barrier t)` if it emits nothing until its input EOFs -- `explain`
  shows that, and it cannot be derived from the type signature.
- `emit` and `finish` are macros — never `#'emit`.
- `do-input` declares its variable `ignorable`; don't add `(declare (ignore ...))`
  inside the body, it isn't a valid declaration position there.
- New stages go in `src/stages.lisp`, new exports in `src/package.lisp`.
- **Pad before painting.** `paint` adds SGR escapes that `~va` counts as
  visible characters, so any alignment has to be computed on the bare string
  first. `visible-width` exists for the same reason in `lineedit.lisp`.
