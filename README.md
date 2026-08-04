# plumb

Thread-and-channel pipelines that carry Lisp objects instead of bytes.
SBCL only (`sb-thread`, `sb-mop`); no external dependencies.

```lisp
(asdf:load-system "plumb")
(asdf:test-system "plumb")     ; 137 assertions
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
| number | a number; `5kb` is still a string (no suffix literals yet) |

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

Colour follows `NO_COLOR`, `TERM=dumb`, and whether output is a terminal, so
piped output stays clean. `--no-edit` falls back to plain input.

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
| `src/process.lisp` | `sh` / `to-sh`: external commands, lifetime, exit status |
| `src/help.lisp` | `help`: the stage registry, listing and detail rendering |
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
