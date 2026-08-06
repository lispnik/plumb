# CLAUDE.md

## What this is

`plumb` — an experiment in shell pipelines built from threads and channels
carrying Lisp objects, instead of processes and byte streams. The **core** is
SBCL only -- no external libraries -- and every dependency lives in an optional
system: `plumb/json` (jzon), `plumb/csv` (cl-csv) and `plumb/crypto` (Ironclad).
The **binary** builds with all of them, so `plumb` on your PATH has everything while
`asdf:load-system "plumb"` and `make test` need nothing outside SBCL.

```
sbcl --eval '(asdf:test-system "plumb")'   ; 491 assertions, core only
make                                       ; dump bin/plumb, with every system
make test-json test-csv test-crypto        ; 60, 35 and 79 -- the optional systems
sbcl --script demo.lisp
ocicl install                              ; restore ocicl/ after a fresh clone
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
- **Watching is lock-free, and `last` retains an object on purpose.**
  `channel-passed` is a `sb-ext:word` bumped with `atomic-incf` in `send`, so an
  observer adds no contention to the path it measures; it counts *before* the
  discard early-return, so a discarding sink is still measurable. `channel-last`
  deliberately holds one object past its natural life, and `close-input` drops
  it with the buffer for the same reason it drops the buffer. Neither is dead
  code to tidy away.
- **`with-output-lock` is where the panel comes down.** `*before-output*` runs
  holding the lock, before any shared-stream write. A repainting panel and
  ordinary output would otherwise scroll over each other; a hook can be one line
  only because that macro is already the single funnel for shared writes.
- **`sb-sys:interactive-interrupt` is a `serious-condition`, not an `error`.**
  This is load-bearing: `guarded`'s `(error (c) ...)` clause does *not* catch
  it, which is why ^C reaches `eval-forms-interruptibly` in the REPL and `main`
  in one-shot mode. Widening either handler to `serious-condition` would put ^C
  back to killing the session.
- **A shared stream needs `with-output-lock`.** A CL stream is not thread-safe
  and fan-out lets several stages print at once; without it two branches
  duplicate and drop each other's lines, differently on every run. Per line for
  `print-items`/`peek`, around the whole render for `table`.
- **sysfs `size` is in 512-byte sectors, always.** Not in
  `queue/logical_block_size`. Multiplying by the logical block size is the
  classic wrong answer and is *correct on any 512-byte device*, so it survives
  casual testing -- the Pi would not have caught it. macOS sizes come from the
  exact parenthetical in `diskutil info` (`(500277792768 Bytes)`) and never from
  `diskutil list`, whose figures are rounded to one decimal.
- **`git` is the one source with no platform fork,** because `git log --format`
  and `git status --porcelain=v2` are git's own documented contracts rather than
  a platform's. Use `%at` (a unix timestamp) and not `%aI`: it saves writing an
  ISO 8601 reader, and `.date` is then a universal time like `ls`'s `.mtime`, so
  one `7d` literal compares against both. Fields are separated by ASCII US,
  which no commit metadata can contain.
- **CSV values stay strings unless asked otherwise.** Guessing types is the bug
  every spreadsheet has: `01234` becomes 1234, `1.10` becomes 1.1, a padded id
  loses its padding. `from-csv :numbers` opts in, and even then converts only
  when the text *round-trips* through the printer, so padded and trailing-zero
  values survive. Note also that a word-mode `nil` is the string `"nil"`, which
  is true -- `to-csv :headers ()` is how you turn a flag off there, and that is
  the price of every bare word being a string, which is what keeps globs working.
- **`from-json` maps `false` and `null` both to NIL, on purpose.** `where
  {.draft}` has to work and an absent key already reads as NIL through `field`.
  Integers stay exact rather than becoming doubles -- a 64-bit id would lose its
  low bits silently. Keys are upcased into keywords because `field` compares
  names case-insensitively everywhere else. `\uXXXX` is UTF-16, so a leading
  surrogate must consume its trailing pair or every emoji decodes to two broken
  halves.
- **`disks` reads a kernel interface on Linux and a *tool* on macOS.** That
  asymmetry is the feature's main risk and should not be papered over:
  `/sys/block` is stable ABI, `diskutil` is a user command whose output has
  changed across releases. Both are cross-checked in the suite against `lsblk -b`
  and `diskutil info`, because a parser of human-facing output fails by
  producing plausible numbers rather than by erroring.
- **`with-command` returns `finish-command`'s value, not the body's.** Reading
  its result gives the exit status. `sh` and `ps` never noticed because they use
  it purely for effect; `linux-usage-table` did, and the symptom was `disks`
  emitting nothing at all on Linux while working fine on macOS. Bind what you
  need inside the body.
- **Metadata comes from one `lstat`, never from opening the file.** `open` needs
  read permission and blocks forever on a FIFO; `ls` used to hang on a
  directory containing one. `lstat` also describes a symlink rather than
  following it, which is right because `glob` does not resolve them.
- **Linux uses `statx`, not `struct stat`.** `struct stat` is laid out
  *differently per architecture* on Linux -- aarch64 is 128 bytes with `st_mode`
  before `st_nlink`, x86-64 is 144 with them swapped -- so hand-writing it means
  one declaration per arch, each needing its own machine to verify. `struct
  statx` is kernel UAPI with one fixed layout everywhere, and it answers
  something `struct stat` cannot: Linux has no `st_birthtime` field at all, but
  `STATX_BTIME` exists and ext4 keeps it. Offsets were confirmed with
  `offsetof(3)` on the target. Note the mask: the kernel says *per file* whether
  a birth time exists, and a filesystem without one must report NIL rather
  than 1970.
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
- **Globbing is POSIX fnmatch plus `**`, and stays that way.** Alternation,
  extglob, brace expansion, zsh's operators and glob qualifiers were all built
  (see `c3411ba`) and then removed: each has a pipeline equivalent, and a
  second query language inside the pattern string is the thing `where` and
  `sort-by` exist to avoid. The same argument as `ps` having no `--sort`.
- **`ls` streams, and must keep streaming.** `map-glob` emits as it walks; a
  downstream `take` closing the channel makes `emit` signal `channel-closed`,
  which unwinds the callback and the walk. The early exit is the existing
  teardown doing its job, not machinery added for it. Do not reintroduce a
  buffer to sort the result: ordering comes from sorting each directory as
  the walk reaches it, and `**` interleaves its two cases per entry so the
  walk is genuinely depth first. Doing the zero-level pass first emits every
  sibling before descending into any, which a final sort used to hide.
- **Stage threads are pooled, and the pool must stay elastic.** `spawn` never
  waits for a free worker: every stage of a pipeline has to be running for any
  of it to progress, so a stage queued behind a busy pool while the stage ahead
  blocks on a full channel is a *deadlock*, not a slow start. The pool is a
  cache of idle threads and nothing more. Bounding it would look like a tidy-up
  and would hang the first pipeline wider than the bound.
- **Fusion is a CPU optimisation, not a latency one** -- measured, so it is not
  re-derived. Three `xform`s over 1M objects: 4 channels 1.121s wall / 4.02s
  CPU, the same work hand-fused to 2 channels 1.149s wall / 2.54s CPU. Stages
  already run concurrently, so channel cost is paid in parallel and removing
  channels bought *nothing* in wall clock. What was real was setup:
  `make-thread` 28us against 2.6us for a pooled lease, and ~113us of a
  four-stage pipeline's 214us. That is why `src/pool.lisp` exists and
  `src/fuse.lisp` does not.
- **Parallelism is opt-in, because the unsafe cases fail silently.** A stage
  declares `(:parallel t)` and `defstage` gives it `&key (workers 1)`; `run`
  then spawns that many threads sharing one input. Nothing about a thunk says
  whether two copies of it are sound -- `take` mutates the constructor's own
  parameter, `uniq`'s `seen` would become per-worker, a barrier is sequential,
  a source would emit everything N times -- and every one of those is a wrong
  answer rather than an error. Do not try to infer it.
- **`channel-consumers` mirrors `channel-producers`,** and `close-input` is
  refcounted the way `close-output` already was: N workers share one input, so
  the first to finish must not SIGPIPE the rest. `abort-input` is separate and
  unconditional because `cancel` means *now* -- against eight workers a
  decrement retires one and leaves seven reading.
- **Both refcounts are thread counts, not stage counts.** `run` sets a
  channel's `producers` from the upstream stage's worker count and `consumers`
  from the downstream one; `:err` gets the total across the pipeline. Getting
  either wrong is a hang, or EOF delivered while somebody is still writing.
- **Type checking happens before any thread is spawned** (`check-pipeline`).
  `T` on the consuming side means any object *type*, not the absence of one, so
  nothing may follow a stage that produces `nil`. Reading it the other way let
  a sink follow a sink, and `explain` reported that pipeline as fine.
  `defstage`'s `(:check FORM...)` is the same rule one level down, for a single
  stage's arguments: the forms run in the constructor, before the declared
  `check-type`s, so a stage can beat "not of type SUPPORTED-DIGEST" with a
  message that says what to type instead.
- **Ironclad is vendored in `ocicl/`,** pinned by a committed `ocicl.csv`.
  Before that, `make crypto` resolved it out of a *neighbouring project* under
  the user's own `(:tree "~/Projects/common-lisp/")` -- it built here and would
  have built nowhere else. The vendored tree is listed before
  `:inherit-configuration` so it wins.
- **The vendored tree must be COMPLETE, not merely present.** Twice a build has
  worked here and nowhere else because ASDF quietly satisfied a missing
  transitive dependency out of a *neighbouring project* under the user's own
  `(:tree "~/Projects/common-lisp/")` -- Ironclad the first time, `cl-ppcre`
  (under cl-csv, via cl-unicode) the second, and both were caught only by
  running on the Pi. `make check-vendored` loads every optional system with the
  inherited registry switched off, which is the cheap way to catch it here.
- **Dependencies are vendored, never resolved at build time.** `ocicl.csv` is
  committed and `ocicl/` is not, so `ocicl install` restores an exact tree and
  the build itself needs no network. Before this, `make crypto` resolved
  Ironclad out of a *neighbouring project* under the user's own
  `(:tree "~/Projects/common-lisp/")` -- it built here and would have built
  nowhere else.
- **Every external library is an optional system; the binary takes them all.**
  `plumb/json`, `plumb/csv` and `plumb/crypto` each hold one library, one source file and one
  test system. That keeps the core loadable and testable with nothing but SBCL,
  *and* keeps `from-json` always present where it matters -- because its value
  is that every `--json` tool becomes a source, which an opt-in binary would
  gut. `build.lisp` loading them is deliberately fatal on failure: a binary
  quietly missing a stage is the same trap as a build silently picking a
  flavour. Each defines into the `plumb` package rather than its own, because
  the reader, `help` and TAB completion all read that one package.

## Invariants to preserve

1. `spawn-stage`'s cleanup must run on all three exit paths — EOF, `(finish)`,
   and error — or teardown stops cascading and pipelines hang.
2. `(finish)` closes the input channel on the way out; the upstream `send` then
   signals `channel-closed`, which the handler treats as *normal* termination.
   Breaking this turns `(list (counter) (take 5))` into an infinite loop.
3. Every test is wrapped in `sb-ext:with-timeout`. Bugs here present as hangs,
   not backtraces — keep new tests time-boxed.
4. Anything that opens a file must refuse a FIFO first. Opening one with no
   writer blocks forever and no downstream stage can time it out; this bit `ls`
   once and `digest` would have inherited it.
5. ^C must abandon the pipeline, not the session. `main` exits 130, which is
   right for `plumb 'expr'` and wrong at a prompt; `eval-forms-interruptibly`
   is what makes the REPL survive it, and unwinding through `each`'s
   `unwind-protect` is all the teardown it needs.
6. There is **one** binary flavour, with every optional system in it. Two
   flavours writing the same path needed marker files under `bin/` so that
   `make`, `make demo` and `make crypto` could not hand you the wrong one, and
   that trap is now gone by construction. `bin/plumb` is still removed before
   each dump, because `program-op` skips the dump when its output is newer than
   its inputs and would otherwise report success without rebuilding.
   `--version` reports `(+crypto +json)` by asking the stage registry, so it
   cannot disagree with what is actually in the image.

## Open work, roughly in priority order

1. **Stage fusion.** `where`, `xform`, `take` and other simple transducers could
   collapse into one thread. Still open, but *demoted*. The cost that motivated
   it -- "not fine when a loop spawns a pipeline per file" -- was thread
   creation, and `src/pool.lisp` now removes that at a fraction of the blast
   radius. What fusion still buys is ~37% CPU on a long pipeline and no wall
   clock at all (measurements above). It would need a per-object step separated
   from the `do-input` loop -- a second way to write a stage -- plus `emit`
   becoming an indirect call, and `watch` would lose its per-stage numbers
   because the channels it reads would be gone. Do not start it without a
   workload that is actually CPU-bound on channel traffic.
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
  `src/crypto.lisp` is the exception on both counts: it exports at load time,
  since its symbols name nothing on a build without Ironclad.
- `birthtime <= mtime` is **not** an invariant. `cp -p` and `rsync -a` create a
  file now and put the old mtime back, so a copied file -- including anything
  rsync'd to a test box -- legitimately has a birth time *later* than its
  mtime. Test the ordering on a file created in place, where it does hold.
- Don't assert the ambient environment in a test either. The `ls` fixture used
  `chmod +x` and asserted `-rwxr-xr-x`, which is true under umask 022 and false
  under Debian's 002; `chmod 755` states what it means.
- Don't assert a census of the repository in a test. `(= 3 (length (glob
  "src/[cf]*.lisp")))` failed the day a source file was added; assert the
  property instead.
- **Pad before painting.** `paint` adds SGR escapes that `~va` counts as
  visible characters, so any alignment has to be computed on the bare string
  first. `visible-width` exists for the same reason in `lineedit.lisp`.
