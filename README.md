# plumb

Thread-and-channel pipelines that carry Lisp objects instead of bytes.
SBCL only (`sb-thread`, `sb-mop`); no external dependencies.

```lisp
(asdf:load-system "plumb")
(asdf:test-system "plumb")     ; 237 assertions
```

```
sbcl --script demo.lisp
make            # dump bin/plumb
make test
```

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

### Globbing

`ls` takes a pattern, so globbing needs no reader syntax at all — `*.lisp` is
already a plain string by the earmuff rule, and `ls` decides what to do with it:

```
$ plumb 'ls src/*.lisp | where {(> .size 10kb)} | sort-by .size :desc'
$ plumb 'ls **/*.lisp | tally'          # ** descends
$ plumb 'ls src/[cf]*.lisp'             # character classes
$ plumb 'ls src/?ield.lisp'             # ? is one character
```

A directory lists its members with or without the trailing slash, a plain file
names itself, and a pattern matching nothing yields nothing rather than an
error. Results are **sorted**, so pipelines built on `ls` are reproducible.

Note that shell `*` and Common Lisp `*` do not mean the same thing — CL's means
*"any name, no type"*, which would silently miss every file with an extension —
so `glob` translates before handing the pattern to `directory`.

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

## Files

| | |
|---|---|
| `src/ansi.lisp` | terminal colour, shared by the prompt and `help` |
| `src/channel.lisp` | bounded FIFO, backpressure, two-sided close |
| `src/field.lisp` | uniform field access, `$` block macro |
| `src/reader.lisp` | word mode: `\|`, `{...}`, `.field`, earmuffs |
| `src/present.lisp` | `present` generic, table rendering |
| `src/stage.lisp` | `defstage`, dynamic ports, `do-input`/`emit`/`finish` |
| `src/pipeline.lisp` | wiring, spawning, teardown, type checking |
| `src/stages.lisp` | `from-list` `counter` `ls` `lines` `where` `xform` `take` `drop` `uniq` `peek` `sort-by` `tally` `accumulate` `to-text` `print-items` `table` |
| `src/process.lisp` | `sh` / `to-sh` / `ps`: external commands and the process table |
| `src/help.lisp` | `help`: the stage registry, listing and detail rendering |
| `src/explain.lisp` | `explain`: pipeline metadata, drawn without running |
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
