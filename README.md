# plumb

Thread-and-channel pipelines that carry Lisp objects instead of bytes.
The core is SBCL only. Every external library lives in an optional system --
`plumb/json` (jzon) and `plumb/crypto` (Ironclad) -- vendored under `ocicl/`,
pinned by a committed `ocicl.csv` and restored with `ocicl install`. **The
binary builds with all of them**, so `plumb` on your PATH has everything.

```lisp
(asdf:load-system "plumb")     ; core only -- nothing outside SBCL
(asdf:test-system "plumb")     ; 491 assertions on macOS, 500 on Linux
```

```
sbcl --script demo.lisp
make            # dump bin/plumb
make test
```

One optional system, `plumb/crypto`, does have a dependency -- Ironclad, for
digests. It is a separate system so that everything above keeps working on a
machine with no way to fetch it. See [Digests](#digests).

## The binary

`make` builds `bin/plumb` with ASDF's `program-op`. It takes two syntaxes, and
a **leading paren decides which**:

```
$ plumb 'ls src/ | where {(> .size 4096)} | take 3'      # word mode
$ plumb '(list (ls "src/") (take 3))'                    # Lisp
```

One rule then makes it a pipeline shell rather than an evaluator — a value that
is a stage, or a list of stages, is *run*, and its output printed; anything
else is just printed.

```
$ plumb 'sh "df -h" | drop 1 | take 2'
$ plumb 'ls src/ | sort-by .size :desc | take 3 | table :columns (list :name :size)'
$ plumb 'counter | take 3'                  # infinite source, bounded consumer
$ plumb 'counter' | head -3                 # exits 0, tears the source down
$ plumb 'help take'
$ plumb -i                                  # or just `plumb` on a terminal
```

### Word mode

| | |
|---|---|
| `\|` | separates stages |
| bare word | a string — `ls src/` passes `"src/"` |
| `{...}` | a block over the current object → `($ ...)` |
| `.name` | reads a field → `(fld :name)`; bare, it is shorthand for `{.name}` |
| `*x*`, `+x+` | a variable, not a string — so globs like `*.lisp` stay strings |
| `:desc` | a trailing keyword is a flag, so it means `:desc t` |
| `(...)`, `#'f` | Lisp, verbatim — as a whole stage or as one argument |
| `> f` `>> f` `< f` | redirection; binds to the whole pipeline, as a shell means it |
| number | `5`, or a suffixed literal — `1kb` `2mb` `1.5gb`, `30s` `5min` `2h` `1d` |

Sizes are **binary** — `1kb` is 1024, the way `ls -h` and `du -h` mean it —
and durations are **seconds**, so they compose with `get-universal-time`, which
is what `.mtime` holds. Minutes are spelled `min`, not `m`: `m` is megabytes
here, and a unit that changed meaning depending on the field you compared it
against would be a silent wrong answer rather than an error.

```
ls src/ | where {(> .size 10kb)}
sh "find . -type f" | take 5
```

`ls` costs **one `lstat` per entry**, and that single call supplies everything:

| | |
|---|---|
| `.name` `.size` `.mtime` `.atime` `.ctime` | |
| `.type` | `:file` `:directory` `:symlink` `:fifo` `:socket` `:character-device` `:block-device` |
| `.mode` `.nlink` `.uid` `.gid` `.user` `.group` `.ino` | `mode-string` renders `-rwxr-xr-x` |
| `.target` | where a symlink points (one `readlink`, symlinks only) |
| `.mtime-nsec` `.atime-nsec` `.ctime-nsec` | the fraction of a second, 0–999999999 |
| `.birthtime` `.blocks` `.blksize` | creation time, 512-byte blocks allocated, block size |
| `.dev` `.dir-p` | `.dir-p` is kept, and equals `(eq .type :directory)` |

`present` says what things are, `ls -F` style — `dir/`, `link@`, `pipe|`,
`socket=`, `executable*`.

It used to call `file-length` on an open stream, which cost `open`+`fstat`+
`close` per file *plus* a `stat` for the mtime. That was four syscalls for two
fields, it lost the size of anything it could not open, and it **hung forever
on a FIFO** — `open` on a named pipe waits for a writer. One `lstat` cannot
block, needs no read permission, and describes the link rather than following
it. Listing 2962 files went from 210–320 ms to 100–140 ms, against a 80 ms
floor for the directory scan alone.

`sb-posix`'s `stat` does not surface sub-second timestamps, `st_blocks` or
Darwin's `st_birthtime`, so `src/stat.lisp` declares the platform's
`struct stat` and calls `lstat` through `sb-alien` — still **one syscall**, just
one that gives up everything it has. Files written inside the same second get
a real order:

```
$ plumb 'ls dir/ | sort-by {(precise-time (fld :mtime) (fld :mtime-nsec))} | …'
a .443382875
b .443475501
c .443545418
```

`.mtime` itself stays whole-second universal time, so `decode-universal-time`
and comparisons against `get-universal-time` keep working; the fraction sits
beside it rather than being folded in. `precise-time` combines them into an
exact **rational** — a double cannot hold a universal time to nanosecond
resolution, so a float would drop the difference exactly where it matters.

A hand-written struct layout is only safe if it is checked, so the test suite
compares every field `sb-posix` also knows — size, mode, inode, uid, gid,
nlink, dev, mtime, atime — against it. A wrong offset fails there on a value
known independently, rather than appearing as plausible nonsense in the
nanosecond fields nothing else can verify. Darwin/arm64 is implemented and
tested; elsewhere it falls back to `sb-posix` with those extra fields `nil`.

Owner and group names are looked up once per distinct id, in a table local to
each `ls`, so a listing costs a handful of lookups regardless of file count.

One behaviour changed: `.size` is now what the filesystem says even for a
directory, where it used to be `nil`. Filter on `.type` rather than relying on
a missing size to mean "not a file".

### Globbing

`ls` takes a pattern, so globbing needs no reader syntax at all — `*.lisp` is
already a plain string by the earmuff rule, and `ls` decides what to do with it:

```
$ plumb 'ls src/*.lisp | where {(> .size 10kb)} | sort-by .size :desc'
$ plumb 'ls **/*.lisp | tally'          # ** descends
$ plumb 'ls src/[cf]*.lisp'             # character classes
$ plumb 'ls src/?ield.lisp'             # ? is one character
```

POSIX `fnmatch`, and no more than that: `*`, `?`, `[a-z]`, `[!a]`, the
character classes `[[:digit:]]` and friends, `[[.a.]]` and `[[=a=]]` (which
degenerate to the literal character, there being no collating locale),
backslash escapes, and the leading-dot rule — `*` skips dotfiles, write `.*`
for those. Plus `**` for recursive descent, which is not POSIX but is what
every modern shell means by it.

Deliberately **not** here: alternation, extglob, brace expansion, zsh's
operators and glob qualifiers. They were built and then removed — every one
has a pipeline equivalent already, and `where`/`sort-by`/`take` say the same
things without a second query language inside the pattern string.

A directory lists its members with or without the trailing slash, a plain file
names itself, and a pattern matching nothing yields nothing rather than an
error.

**`ls` streams.** It emits as it walks rather than globbing the tree first, so
a downstream `take` stops the walk instead of paying for a tree it will not
look at:

```
ls "/usr/share/**/*" | take 3     380 ms  ->  20 ms      (15,732 files)
ls "/usr/share/**/*" | tally      370 ms      unchanged
```

Ordering therefore comes from sorting each directory as the walk reaches it,
depth first, rather than sorting the finished result — a streamed result has no
end at which to sort. It is still fully reproducible, and on a real tree it is
the *same* order: over `/usr/share/man`'s 2995 entries the streamed output is
byte-identical to what the sorting version produced. The two can differ only
where a directory name is a prefix of a sibling file name, which puts
`c/d.txt` before `c.txt`.

`glob` yields the same order, collected — the two cannot disagree, since both
come from `map-glob`.

Globbing does **not** go through CL's `directory` and pathname patterns, which
got five things wrong — three of them silently. `[a-c]` was the literal set
`{a,-,c}`, so ranges skipped members; `[!a]` was `{!,a}`, so negation matched
the *opposite*; `*` matched dotfiles; matching was case-sensitive even on a
case-insensitive filesystem; and files whose names contained `*` or `[`
**vanished from `ls` entirely** — `directory` returned them with the
metacharacter as a pattern object, `file-namestring` re-escaped it, `lstat` on
the escaped path failed, and the entry was dropped without a word.

So `src/glob.lisp` reads entries with `readdir` as plain strings, matches them
with its own fnmatch, and builds pathnames only at the end with
`parse-native-namestring`, which treats `*` as the character it is.

Descending a named component **follows** symlinks, as a shell does — `/tmp` is
itself a symlink on macOS. `**` does **not**, so a link pointing back up a tree
cannot recurse forever.

There is no glob-in-stage-position shorthand: write `ls *.lisp`, not `*.lisp`.
Same reasoning as `sh` — an unknown first word stays an error.

**Dotfiles need quoting.** A bare `.name` is the field-accessor shorthand, and
`.gitignore` is lexically identical to `.size`, so it cannot be told apart:

```
$ plumb 'ls .gitignore'      # parses as (ls ($ (fld :gitignore)))
plumb: A bare .name is a field accessor, so a dotfile needs quoting: …
$ plumb 'ls ".gitignore"'    # this is the way
```

A lone `.` is below the two-character threshold, so `ls .` and `ls ".."` both
mean what you expect.

Suffixes are two narrow substitutions, not a reader macro — catching `1kb` at
read time would mean owning the digit characters and reimplementing CL's number
syntax. One consequence: inside a block the rewrite does not respect `quote`,
so `'(1kb)` becomes `'(1024)` and a literal `.size` symbol cannot be written.

Blocks, forms and strings may each contain a `\|`, so splitting on the pipe is
depth-aware rather than a first pass. The reader is a source-to-source pass —
it emits a form and hands it to the ordinary evaluator, so stages, `present`,
teardown and `help` all work on word mode unchanged:

```
ls src/ | where {(> .size 1024)} | sort-by .size :desc | take 5
```
```lisp
(list (ls "src/") (where ($ (> (fld :size) 1024)))
      (sort-by ($ (fld :size)) :desc t) (take 5))
```

Redirection binds to the whole pipeline rather than the stage beside it:

```
ls src/*.lisp | xform .name > names.txt      # (to-file "names.txt")
counter :limit 3 >> log.txt                  # :if-exists :append
< names.txt | where {(search "cli" .text)}    # (from-file "names.txt")
```

`>` and `<` end a word, so `ls >out.txt` splits without spaces. A `>` inside a
block still means greater-than — blocks are scanned whole, so the redirection
pass never sees inside one.

There is deliberately **no fallback to an external command** — an unknown first
word is an error, not an exec, so a typo'd stage name says so. Use `sh`.

Single-quote the whole thing — `$`, `*` and `|` all mean something to your
shell too.

### The REPL

`plumb -i`, or bare `plumb` on a terminal, gets a line editor with emacs keys.

| | |
|---|---|
| move | `C-a` `C-e` `C-b` `C-f` `M-b` `M-f`, arrows, Home/End, ctrl-arrows |
| edit | `DEL` `C-d` `C-h` `C-t` `C-k` `C-u` `C-w` `M-d` `M-DEL` `C-y` `C-g` |
| case | `M-u` `M-l` `M-c` |
| history | `C-p` `C-n`, up/down, `M-<` `M->` |
| complete | `TAB` — stage names, operators and variables |
| other | `C-l` clear, `C-c` abandon the line, `C-d` on an empty line exits |

Word motion is symbol-aware, so `M-b` steps over `*default-capacity*` in one
go; `C-w` keeps readline's whitespace rule. An unbalanced form keeps reading on
a `...` continuation line, and pasting a multi-line form works.

The prompt is `*prompt*` — a string, or a function of no arguments returning
one. The default shows the input number, the directory `ls` would list, and a
marker that turns red when the last form failed:

```lisp
(setf ple:*prompt* "λ ")                          ; a string
(setf ple:*prompt* (lambda () (format nil "~a> " (length ple:*history*))))
```

`TAB` completes: a single candidate is inserted outright, several reduce to
their common prefix, and `TAB` again lists them. Candidates come from the same
two places `help` reads — the stage registry and the package's export list — so
completion cannot drift out of step with the documentation.

History persists to `~/.plumb_history` (`ple:*history-file*`, `nil` to disable).
It is **appended** rather than rewritten at exit, so a crash keeps what you
typed and two sessions interleave instead of clobbering each other; the file is
trimmed on load once it grows past twice `*history-limit*`. A piped `plumb -i`
is a script, so it does not write to it.

Colour follows `NO_COLOR`, `TERM=dumb`, and whether output is a terminal, so
piped output stays clean. `--no-edit` falls back to plain input.

### explain

`explain` draws a pipeline **without running it** — constructing a stage spawns
nothing, so all the metadata is there while the pipeline is still inert:

```
$ plumb 'explain ls src/*.lisp | sort-by .size :desc | take 5 | table'

pipeline of 4 stages, 3 channels, 4 threads

  ls pattern="src/*.lisp"            source     nothing → :objects
  │ channel, capacity 64
  sort-by key=fn desc=t              transform  :objects → :objects
      ⋯ barrier: emits nothing until its input ends
  │ channel, capacity 64
  take n=5                           transform  :objects → :objects
  │ channel, capacity 64
  table stream=stream max-width=40   sink       :objects → nothing

types check; RUN would start it.
```

Arguments are the values the constructor was actually called with, not the
lambda list. Drawing an **invalid** pipeline is the point rather than an edge
case — `run` refuses one and names the pair that disagreed, while `explain`
shows the whole shape with the bad joint marked where it sits:

```
$ plumb 'explain from-list (list 1) | to-text | where #'evenp'
  from-list items=(1)   source     nothing → :objects
  │ channel, capacity 64
  to-text               transform  :objects → :bytes
  ✗ to-text produces :bytes but where consumes :objects
  where pred=fn         transform  :objects → :objects

1 type error -- RUN would refuse this pipeline.
```

`explain` is a **reserved first word** that wraps the whole pipeline, the way
bash's `time` does — a closed list of keywords, not a general prefix mechanism.
From Lisp it is an ordinary function: `(explain (list (ls) (take 3)))`.

`plumb --help` documents the command line; `help` documents the language:

```
$ plumb help              # every stage and operator, grouped
$ plumb '(help take)'     # detail for one built-in
```

```
take  (transform)

  (take n)
    n must be (integer 0)

  consumes :objects
  produces :objects
  ports    :out :err

  Pass the first N objects, then stop the whole upstream.
```

`help` is a macro so the name needs no quoting, and bare `help` is a symbol
macro so `plumb help` works from a shell. Stages come from a registry that
`defstage` fills in; everything else is read off the package's export list, so
a new export appears without anyone updating a list by hand.

The calling thread is the pipeline's consumer, so
a downstream `head` closing the pipe reaches the source through exactly the
same backpressure `take` uses internally. Exit status: 0 ok, 1 evaluation or
pipeline error, 2 usage error, 130 interrupt.

## watch

`explain` says what a pipeline *is*; `watch` says what it is **doing**. Same
layout, live numbers:

```
$ plumb 'watch ls "/usr/share/**/*" | where {(eq .type :file)} | digest :md5 | tally'

watching 4 stages
  ls      pattern="/usr/share/**/*"                    1,066 objs  2.1k/s
  │ ██████████ 64/64  last: P-ekans-X3_M-HRPN_V-m.txt@
  where   pred=fn                                        966 objs  1.9k/s
  │ ██████████ 64/64  last: BCM4388C2_EVTv3_PCIE.bin
  digest  algorithm=:md5 external-format=:utf-8          900 objs  1.8k/s
  │ ·········· 0/64   last: 7bbc2a37…  /usr/share/firmware/…
  tally    ⋯ barrier                                       0 objs
  │ ·········· 0/64
```

Read that top to bottom: two channels pinned at 64/64 in front of `digest`,
which has emitted less than it received, and a barrier that has emitted
nothing. The bottleneck is named, without a profiler.

Everything shown comes off the channels themselves — `channel-count` against
`channel-capacity` for occupancy, `channel-passed` for throughput,
`channel-last` for the most recent object. `passed` is a `sb-ext:word` bumped
with `atomic-incf` in `send`, so watching adds no lock to the hot path, and it
is counted *before* the discard early-return so a pipeline whose sink discards
is still measurable.

Two forms. The word wraps the whole pipeline, the way `explain` does; the
**stage** taps one point, the way `peek` does:

```
$ plumb 'watch ls "**/*" | where {(> .size 1mb)} | tally'    # every stage
$ plumb 'ls "**/*" | watch | where {(> .size 1mb)} | tally'  # one point
```

Both register into one registry that a single watcher thread draws, so the two
cannot drift apart. From Lisp they are `watch-pipeline` and `(watch)` — one
name cannot be both a pipeline runner and a stage, and the surface syntax is
the thing worth keeping uniform.

The panel goes to **stderr**, repainted in place, so stdout stays exactly what
it was:

```
$ plumb 'watch ls src/ | take 5' | wc -l        # 5 -- panel on the terminal
$ plumb 'watch ls src/ | take 5' 2>/dev/null    # data only, no panel
$ plumb 'watch ls src/ | take 5' > /dev/null    # panel only
```

Repainting and ordinary output cannot collide, because the panel takes itself
down before anything else writes: `with-output-lock` is already the one place
every shared-stream write funnels through, so a single hook there is the whole
mechanism. When stderr is not a terminal there is no cursor motion at all —
just one static block of totals at the end.

## Pooled stage threads

A stage still gets a thread of its own, but it is **leased, not created**.
`sb-thread:make-thread` costs ~28 µs here and handing work to a parked worker
~2.6 µs, so a four-stage pipeline used to spend about half its 214 µs of setup
just making threads — which is what made a pipeline-per-file loop expensive.

```
500 four-stage pipelines   0.107s  ->  0.02s
1M objects, four stages    1.121s  ->  0.957s   (CPU 4.02s -> 3.09s)
```

Running 50 pipelines — 150 stages — creates **three** threads.

The pool is a *cache of idle threads, never a limit on how many stages can
run*. `spawn` never waits: if nothing is parked it makes one. That is not an
optimisation but a correctness requirement, because every stage of a pipeline
has to be running for any of it to progress — a stage queued behind a busy pool
while the stage ahead of it blocks on a full channel is a deadlock. Workers are
renamed per task, so `plumb:ls` and `plumb:digest/3` still appear in backtraces.

This is deliberately *not* stage fusion, which is what open work item 1
proposed. Measured, fusion buys ~37% less CPU on a long pipeline and **no
wall-clock latency at all** — stages already run concurrently, so the channel
cost is paid in parallel. Pooling attacks the cost measurement actually found,
and leaves `emit`, the stage protocol and one-thread-per-stage alone.

## workers

One thread per stage is the default. A stage that declares itself safe can run
under several, sharing one input channel:

```
$ plumb 'ls "/usr/share/**/*" | where {(eq .type :file)} | digest :md5 | tally'
15729                                                              # 4.22s
$ plumb 'ls "/usr/share/**/*" | where {(eq .type :file)} | digest :md5 :workers 8 | tally'
15729                                                              # 0.83s
```

5× on 8 cores, same 15,729 objects. No scheduler was written for this: `recv`
already dequeues under the channel's mutex, so N workers pulling from one
channel *is* the work distribution.

**Output is in completion order, not input order.** That is a promise, not an
accident — no reorder buffer, no sequence numbers threaded through your objects,
no head-of-line blocking when one worker gets a slow file. Add a `sort-by` when
order matters. `explain` says so on any stage you have given workers to:

```
  digest algorithm=:md5   transform  :objects → :objects
      ×8 workers: output is in completion order, not input order
```

**It is opt-in per stage**, declared with `(:parallel t)` next to `(:barrier t)`,
because the unsafe cases fail *silently*: `take` and `drop` mutate the
constructor's own parameter, `uniq`'s seen-set would quietly become per-worker,
a barrier is sequential by definition, and a source would emit everything N
times. So `take 5 :workers 4` is an error rather than a wrong answer. Today
`xform`, `where` and `digest` declare it; for `xform` and `where` the guarantee
is inherited from the function you pass, not granted by the stage.

**When it does not pay.** Only per-object work that dominates channel overhead.
`where {(> .size 1kb)}` costs less than one mutex acquisition, so workers make
it slower — the single channel mutex is the ceiling. This is the opposite lever
from stage fusion, and they are complementary: fuse the cheap stages,
parallelise the expensive one.

## Shape

A **stage** is a closure with a type signature. It contains no concurrency at
all — ports arrive through dynamic bindings that the runner establishes, so a
stage body is an ordinary loop:

```lisp
(defstage where ((pred (or function symbol)))
  (:consumes :objects) (:produces :objects)
  (let ((pred (ensure-fn pred)))
    (do-input (x)
      (when (funcall pred x)
        (emit x)))))
```

A **pipeline** wires n stages with n−1 bounded channels and spawns a thread
each. `run` returns immediately; `collect-pipeline` and `each` drain in the
calling thread, so backpressure reaches all the way back to the source.

```lisp
(each (list (ls "/usr/src/")
            (where ($ (> (fld :size) 1024)))
            (sort-by ($ (fld :mtime)) :desc t)
            (take 10))
      #'print)
```

## ps

```
$ plumb 'ps | where {(> .rss 250mb)} | sort-by .rss :desc | table :columns (list :pid :name :rss :pcpu)'
  pid  name                               rss  pcpu
93597  com.apple.WebKit.WebContent  675528704   0.0
70856  IntelliJ                     634601472   4.0
41050  claude                       381157376   8.3
```

`ps` emits a `process` per running process — all of them, as `ps ax` does — with
`pid ppid user state pcpu pmem rss vsz etime tty name command args`.

**There are no selection options, on purpose.** Narrowing is `where`, ordering
is `sort-by`, grouping is `tally`. That is the whole argument for objects over
text: `ps(1)` needs `-u`, `-e`, `--sort` and `-o` because its output is a
formatted string, and once columns keep their types none of that has to exist.

```
plumb 'ps | tally :key .name | sort-by {(fld :count)} :desc | take 5 | table'
plumb 'ps | where {(string= .user "root")} | tally'
```

`rss` and `vsz` are in **bytes**, not the kilobytes `ps` prints — `ls` reports
`.size` in bytes, and a unit that changed meaning depending on which source
produced the object would undo the reason for having objects. So one `500mb`
literal means the same thing against both.

The data comes from `ps(1)`; what plumb adds is that it arrives as objects. A
native implementation would mean `/proc` on Linux and `sysctl` plus `libproc`
on macOS — two lots of platform FFI to obtain what `ps` already prints.

## Sources

Beyond `ls` and `ps`, four more things the system knows, as objects:

```
$ plumb 'env | where {(search "PATH" .name)} | table'
$ plumb 'commits | where {(> .date (- (get-universal-time) 7d))} | tally :key .author'
$ plumb 'changes | where {(eq .status :untracked)} | print-items'
$ plumb 'handles | where {(eq .state :listen)} | table :columns (list :command :name)'
```

**`env`** is the process's own environment — no subprocess, nobody's output to
parse. **`commits`** and **`changes`** are `git log --format` and
`git status --porcelain=v2`, which are git's *own documented contracts* and so
identical on every platform: there is no `#+darwin` in `src/git.lisp` and there
should never be. **`handles`** is `lsof -F`, a field format built for parsing,
present on both platforms with the same flags — and since a unix descriptor is
not only a file, it covers sockets and pipes too, which is why there is no
separate `connections` stage.

### from-json / to-json

The one that isn't a source at all, and matters most:

```
$ plumb 'sh "ip -j addr" | from-json | where {(string= .operstate "UP")} | table'
$ plumb 'sh "gh pr list --json number,title" | from-json | where {(> .number 100)}'
$ plumb 'sh "docker ps --format json" | from-json :lines | table'
```

Every modern CLI already speaks JSON, so one parser turns all of them into
sources at once rather than a stage per tool. `ip -j addr | from-json` gives
network interfaces as objects with no new code at all.

`to-json` is the other half, and serialises **any** plumb object -- it is driven
by `fields`/`field`, the same thing that makes `table` work on everything, so a
`file-entry`, a `process`, a `commit` or a type you add later all just work:

```
$ plumb 'ls "src/*.lisp" | to-json > files.json'
$ plumb 'ps | where {(> .rss 500mb)} | to-json :pretty'
$ plumb 'ls "src/*" | to-json' | jq -r '.[].name'
```

Parsing is `com.inuoe.jzon`, through its streaming event API rather than
`jzon:parse` -- which returns a hash table whose order is unspecified, so
`table`'s columns would shuffle between runs, and whose keys are strings as
written, so `.name` would not reach `"Name"`. The mapping is chosen
for a shell: objects become plists with upcased keyword keys so `.name` works
and `table` can find its columns; `true` is `T`; **`false` and `null` are both
`nil`**, deliberately, so `where {.draft}` reads the way you expect. Integers
stay exact — a 64-bit id turned into a double would silently lose its low bits,
and these documents are mostly ids. A top-level array is spread into its
elements; `:lines` parses JSON Lines instead and streams.

Malformed input signals with the character position rather than returning
something plausible: a shell that quietly accepted truncated JSON would give
wrong answers instead of no answer.

## disks

Block devices as objects — every disk, partition and volume, mounted or not:

```
$ plumb 'disks | where {(eq .type :disk)} | table :columns (list :name :size :model)'
name     size          model
disk0    500277792768  APPLE SSD AP0512Z

$ plumb 'disks | where {.mount-point} | table :columns (list :name :size :used :fs-type :mount-point)'
name               size          used  fs-type  mount-point
disk3s1s1  494384795648   12644925440  APFS     /
disk3s5    494384795648  438488342528  APFS     /System/Volumes/Data
```

Sizes are in **bytes**, like `ls`'s `.size` and `ps`'s `.rss`, so one `100gb`
literal means the same thing against any of them. No selection options, for the
reason `ps` gives: narrowing is `where`, ordering is `sort-by`.

**The two platforms are read very differently, and are not equally
trustworthy** — worth knowing before relying on it:

| | Linux | macOS |
|---|---|---|
| source | `/sys/block` | `diskutil info -all` |
| kind | kernel-stable interface | a user-facing *tool* |
| cost | plain file reads, no subprocess | one subprocess |
| root | not needed | not needed |

The macOS half is the fragile one: there is no sysfs, `/dev/disk*` is
`root:operator` so the ioctl route needs privileges, and IOKit would mean a
large alien surface over CoreFoundation. Parsing `diskutil` is the best
unprivileged source there, and it is the part most likely to rot — its output
has changed across releases. Both halves are cross-checked in the suite against
`lsblk -b` and `diskutil info` respectively, because a parser of human-facing
output fails by producing *plausible* numbers.

**Every field name is the same on both platforms** — it is one struct, so
`fields` returns an identical 21-key list either way. What differs is which are
populated: `.major` `.minor` `.rotational` `.start` are Linux-only, `.content`
`.protocol` `.internal` are macOS-only, and `.used`/`.available` are set only
where the device is mounted. `.virtual` has a real source on both — a disk
image on macOS, an attached loop device on Linux.

One caveat on the booleans. `.read-only`, `.removable`, `.rotational`,
`.internal` and `.virtual` are `nil` both for *false* and for *this platform
cannot say*, and nothing distinguishes the two. `where {(not .rotational)}`
therefore also matches devices whose rotational state is unknown.

What is deliberately **not** unified: macOS synthesised APFS containers and
`Physical Store` have no Linux analogue, and Linux device-mapper, LVM, `md` and
`loop` have none on macOS. `.parent` expresses both and `.type` is coarse
(`:disk` `:partition` `:volume` `:loop` `:ram`) rather than a union of two
platform vocabularies. On an APFS volume `.size` is the whole container,
because that is what an APFS volume actually has — no fixed extent of its own.

## Fan-out

`tee` sends every object down each of its branches as well as onward, so one
stream feeds several pipelines:

```lisp
(list (ls "src/")
      (tee (list (where ($ (fld :dir-p))) (to-file "dirs.txt"))
           (list (tally)))
      (sort-by ($ (fld :size)) :desc t))
```

A branch is an ordinary list of stages, run with `run :input` — the channel a
pipeline reads from instead of starting at a source. **A branch that stops
early is dropped and the rest carry on**; that independence is the whole point.
A `take` *downstream* of a `tee` still tears the source down through it.

Printing from parallel branches is safe: `print-items`, `peek`, `table` and the
CLI's printer all hold one output lock, at line granularity for the first three
and around the whole render for `table`. Without it two branches duplicated and
dropped each other's lines, differently on every run — a CL stream is not
thread-safe, and fan-out is what made that reachable.

Objects are **shared** with the branches, not copied. Nothing can deep-copy an
arbitrary Lisp object correctly, and every stage here produces new values
rather than mutating. Note what sharing means: `tee` sends to the branches and
emits onward concurrently, so a branch that mutates is a data *race*, not
merely a visible change.

Copying is therefore a **stage**, not a flag — explicit and composable:

```lisp
(tee (list (xform #'copy-file-entry) (xform #'mutate!)))
```

### Named ports

`tee` is one stream to many. The other direction — **many streams out** — is a
stage declaring extra output ports, which `run` wires to their own branches:

```lisp
(defstage route ((pred (or function symbol)))
  (:consumes :objects) (:produces nil) (:ports :yes :no)
  (let ((pred (ensure-fn pred)))
    (do-input (x)
      (try-emit x (if (funcall pred x) :yes :no)))))
```
```lisp
(run (list (ls "src/") (route ($ (> (or (fld :size) 0) 10kb))))
     :ports (list :yes (list (xform ($ (fld :name))) (to-file "big.txt"))
                  :no  (list (tally))))
```

Three things this settled:

**Ports carry their own types.** `:produces` describes `:out` alone, so a named
port declares its own — `(:ports (:yes :bytes) :no)`, defaulting to `:objects`.
Without that the graph would be untyped exactly where it branches. Branch heads
are checked before any thread starts, and the error names the port:

```
ROUTE's :YES port carries :OBJECTS but COUNTER consumes NIL.
```

**`try-emit`, not `emit`.** `emit` is deliberately strict — a closed reader
signals `channel-closed`, which is precisely how `take` stops an infinite
source. A routing stage wants the opposite, since one branch ending must leave
the others running, so it uses `try-emit` and gets `nil` instead.

**An unwired port is discarded, not missing.** `emit` to it succeeds and the
objects go nowhere, rather than erroring inside a thread — and `explain` says
so, so it can't puzzle you silently:

```
  route pred=fn      sink       :objects → nothing
                     ├─ yes (:objects) → tally
                     ├─ no (:objects) → discarded, no branch
```

Port names are one flat namespace per `run`. One level of demux is what a shell
wants; deeper nests by putting a routing stage inside a branch.

## The four channel operations

| | |
|---|---|
| `(send ch obj)` | blocks while full; signals `channel-closed` if the consumer is gone |
| `(recv ch)` | blocks while empty; returns `(values obj t)` or `(values nil nil)` at EOF |
| `(close-output ch)` | producer: EOF |
| `(close-input ch)` | consumer: SIGPIPE |

Two values from `recv` so `nil` stays a legal payload. Two independent close
flags because they mean opposite things and travel in opposite directions.

## Teardown

Everything difficult lives in one `unwind-protect` in `spawn-stage`, whose
cleanup runs on all three exit paths:

- **EOF** — `do-input` returns, outputs close, EOF cascades *forward*.
- **`(finish)`** — `take` throws, cleanup closes its input, the upstream `send`
  signals `channel-closed`, that stage's handler treats it as normal
  termination and closes *its* input. Teardown cascades *backward* to the
  source. This is why `(list (counter) (take 5))` terminates against a source
  that emits integers forever, with no laziness in the data representation.
- **Error** — same teardown, but the condition object is first sent out the
  `:err` port and recorded on the pipeline.

`cancel` is Ctrl-C: close every channel from the consumer side and let the
cascade run.

## Errors are objects

A failing stage yields a live condition, not a string on fd 2:

```lisp
(let* ((err (make-channel))
       (pipe (run (list (from-list '(1 2 0 4))
                        (xform (lambda (n) (/ 100 n))))
                  :err err)))
  (join pipe)
  (recv err))   ; => #<DIVISION-BY-ZERO>
```

Downstream stages can therefore `where` on condition *type*.

## Type checking before launch

Stages declare what they consume and produce, and `run` validates adjacency
before spawning a single thread:

```lisp
(run (list (from-list '(1)) (to-text) (where #'evenp)))
;; => TO-TEXT produces :BYTES but WHERE consumes :OBJECTS.
```

`:bytes` marks the external-process boundary; `lines` and `to-text` are the
adapters across it.

## External commands

`sh` starts a pipeline from a command, `to-sh` ends one in a command:

```lisp
(list (sh "git log --oneline") (where ($ (search "fix" (fld :text)))) (take 10))
(list (ls "src/") (xform ($ (fld :name))) (to-sh "wc -l"))
```

A **string** runs under `/bin/sh`, so pipes and globs work. A **list** is
exec'd directly, with no shell to quote against — use it whenever an argument
came from somewhere else:

```lisp
(sh (list "grep" "-n" pattern file))     ; pattern is never re-split
```

Two things the hand-rolled `run-program` version could not do:

- **The child is killed when its reader goes away.** `(list (sh "yes") (take 3))`
  returns three objects immediately and leaves nothing running. The `take`
  closes the source's output, the `send` inside signals `channel-closed`, and
  the `unwind-protect` in `with-command` reaps the child on the way out. Without
  it the child survives until the whole image exits.
- **A non-zero exit is a condition.** `command-failed` carries the command, the
  exit code and the captured stderr, and travels the same `:err` port every
  other stage error does:

```
$ plumb '(list (sh "ls /nope"))'
ls: /nope: No such file or directory
plumb: Stage SH failed: command "ls /nope" exited 1: ls: /nope: No such file...
exit 1
```

`:on-exit :ignore` when a non-zero exit is expected, and `:stderr :inherit`
when you would rather see the child's stderr live than have it captured.

A mid-pipeline filter — objects in, objects out, through a command — is not
provided; see *Not done yet*.

## Presentation

`present` is a generic returning **one line** for an object — the rule every
downstream tool depends on, in one place rather than re-derived by each printer:

```lisp
(present (make-file-entry :name "src" :dir-p t))   ; => "src/"
(present (make-line :text "hello"))                ; => "hello"
```

So `ls` reads like `ls`, and a shell command is just its output:

```
$ plumb '(list (ls "src/") (take 3))'      $ plumb '(list (sh "df -h") (take 2))'
ansi.lisp                                  Filesystem   Size  Used Avail …
channel.lisp                               /dev/disk3s1 460Gi  12Gi  14Gi …
cli.lisp
```

`table` gives the full view. It buffers, so it is a barrier — a column cannot
be sized until the last row arrives, exactly as `sort-by` cannot sort until
then. Columns default to the union of `fields` across the rows; numbers
right-align; `nil` renders absent rather than as the word "nil":

```
$ plumb '(list (ls "src/") (sort-by ($ (or (fld :size) 0)) :desc t) (take 3)
               (table :columns (list :name :size)))'
name            size
lineedit.lisp  17802
cli.lisp       14942
help.lisp      11357
```

`table :transpose` turns it on its side — field names become row headings and
each record grows rightward as its own column. That is how a wide record
becomes readable; `ps` has thirteen fields and one process does not fit across
a terminal as a row:

```
$ plumb 'ps | take 1 | table :transpose'
pid      1
ppid     0
user     root
state    Ss
rss      15482880
etime    06-10:28:28
name     launchd
command  /sbin/launchd
```

Useful for few records, as the name suggests — several sit side by side with
nothing between them:

```
$ plumb 'ps | sort-by .rss :desc | take 3 | table :transpose :columns (list :pid :user :rss :name)'
pid   70856      41050      2543
user  mkennedy   mkennedy   mkennedy
rss   513097728  369934336  294469632
name  IntelliJ   claude     com.apple.WebKit.WebContent
```

Everything is left-aligned here, unlike a normal table: a column now holds one
*record*, so its values are heterogeneous — an integer `pid` beside a string
`user` — and right-aligning some rows and not others inside one column reads as
ragged. `max-width` also defaults to `nil` rather than 40, since transposing is
usually how you go to read a long value in full.

Give a type its own look with a method:

```lisp
(defmethod present ((row my-row)) (format nil "~a <~a>" (name row) (id row)))
```

## Fields

`field` reads a keyword out of plists, alists, hash tables, structs and CLOS
instances alike, so `.size` in a shell reader can be one accessor. `$` is the
block macro a `{...}` reader would expand to:

```lisp
($ (> (fld :size) 1024))
;; == (lambda (it) (> (field it :size) 1024))
```

## Digests

`plumb/crypto` is the one system with an external dependency. It adds two
stages on top of Ironclad, and it is separate for exactly that reason: `make`,
`make test` and `bin/plumb` must keep working where Ironclad cannot be
installed.

```
make                   # bin/plumb, with plumb/json and plumb/crypto in it
make test-crypto       # 79 assertions
make test-json         # 60 assertions
plumb --version        # plumb 0.1.0 (+crypto +json)
```

Ironclad and its dependencies are **vendored** in `ocicl/`, pinned by
`ocicl.csv` (committed; the unpacked tree is not). So the build needs no
network, no Quicklisp and no dependency manager — and, more to the point, it
cannot quietly resolve Ironclad out of some unrelated checkout that happens to
be on your ASDF source registry. Refresh with `ocicl install ironclad`.

```lisp
(asdf:load-system "plumb/crypto")
```

A digest is an object, not a line of text -- it carries the hex, the raw
octets, the source, and the object it was computed from, so the rest of the
pipeline can still see the file:

```
$ plumb 'ls "src/*.lisp" | digest :sha256 | print-items'
$ plumb 'ls "**/*" | where {(eq .type :file)} | digest :md5 | sort-by .hex | table'
$ plumb 'digests | table'                   # the 60 algorithms Ironclad has
$ plumb 'digests | where {(= .length 32)} | table'
```

`present` renders a digest the way `shasum(1)` writes a line -- hex, two
spaces, name -- so the first of those diffs clean against `shasum -a 256`.

Three details are deliberate:

- **The algorithm is checked when the stage is built**, not when it runs, so a
  typo is an error at the prompt rather than a condition inside a thread four
  hundred files in. `digest :sha257` says so and points at `digests`.
- **A file that cannot be hashed does not vanish.** It comes through as a
  digest with a `nil` hex and a `digest-failed` in its `.error` slot, and the
  condition also goes out the `:err` port. A checksum listing that silently
  omits the files you would most want to know about is worse than useless;
  filter on `.error` to separate them.
- **A directory, a device and above all a FIFO are refused before the open.**
  Opening a FIFO with no writer blocks forever, and nothing downstream can time
  that out -- the same trap `ls` hit before it stopped calling `file-length`.

Adding the system adds real built-ins, not a second-class namespace:
`help digest` describes it, TAB completes it, `explain` draws it. That is why
`src/crypto.lisp` defines into the `plumb` package -- the reader, `help` and
completion all read that one package.

## Files

| | |
|---|---|
| `src/ansi.lisp` | terminal colour, shared by the prompt and `help` |
| `src/stat.lisp` | one `lstat`/`statx` through `sb-alien`: nanoseconds, blocks, birthtime |
| `src/glob.lisp` | POSIX globbing over `readdir`, not CL pathname patterns |
| `src/channel.lisp` | bounded FIFO, backpressure, two-sided close |
| `src/field.lisp` | uniform field access, `$` block macro |
| `src/reader.lisp` | word mode: `\|`, `{...}`, `.field`, earmuffs |
| `src/present.lisp` | `present` generic, table rendering |
| `src/stage.lisp` | `defstage`, dynamic ports, `do-input`/`emit`/`finish` |
| `src/pool.lisp` | stage threads leased from a cache instead of created |
| `src/pipeline.lisp` | wiring, spawning, teardown, type checking |
| `src/stages.lisp` | `from-list` `counter` `ls` `lines` `where` `xform` `take` `drop` `uniq` `peek` `sort-by` `tally` `accumulate` `to-text` `print-items` `table` |
| `src/process.lisp` | `sh` / `to-sh` / `ps`: external commands and the process table |
| `src/blockdev.lisp` | `disks`: /sys/block on Linux, `diskutil` on macOS |
| `src/git.lisp` | `commits` / `changes`: git's own stable formats |
| `src/json.lisp` | `from-json`: a JSON reader, so every --json tool is a source |
| `src/crypto.lisp` | `digest` / `digests`, on Ironclad -- the `plumb/crypto` system |
| `src/help.lisp` | `help`: the stage registry, listing and detail rendering |
| `src/explain.lisp` | `explain`: pipeline metadata, drawn without running |
| `src/watch.lisp` | `watch`: the same shape, live -- occupancy, throughput, last object |
| `src/lineedit.lisp` | raw-mode line editor: emacs keys, history, prompts |
| `src/cli.lisp` | the `plumb` executable: argument parsing, evaluation, REPL |
| `build.lisp`, `Makefile` | `program-op` build of `bin/plumb` |

## Not done yet

- **Fusion.** Simple transducer stages (`where`, `xform`, `take`) could collapse
  into a single thread. Fine at 6 threads per pipeline; not fine when a loop
  spawns a pipeline per file.
- **Fan-out.** Pipelines are linear. Named extra output ports exist in the
  stage struct but `run` only wires `:out` and `:err`; `tee` needs a graph
  builder and a copy-on-fanout policy, since objects crossing a channel are
  shared references.
- **External processes, the rest of it.** `sh` and `to-sh` cover the source and
  sink shapes with real teardown and exit-status propagation. A mid-pipeline
  filter would need a helper thread inside the stage (concurrent read/write, and
  `recv` cannot be selected on), and there is no PTY path yet.
- **Presentation.** `print-items` uses `princ-to-string`. A real shell wants a
  `present` generic with table rendering, and object identity retained per
  screen region.
