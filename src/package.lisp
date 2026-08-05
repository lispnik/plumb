;;;; package.lisp

(defpackage #:plumb
  (:use #:cl)
  (:nicknames #:pb)
  (:export
   ;; channels
   #:channel #:make-channel #:channel-p #:channel-passed #:channel-last
   #:channel-count #:channel-capacity #:channel-name
   #:send #:recv #:close-output #:close-input #:abort-input
   #:channel-producers #:channel-consumers
   #:channel-closed #:channel-closed-channel
   #:*default-capacity*
   ;; fields
   #:field #:fields #:$ #:fld
   ;; stages
   #:stage #:stage-p #:stage-name #:stage-args #:stage-consumes #:stage-produces #:stage-barrier
   #:stage-parallel #:stage-workers
   #:defstage
   #:*input* #:*outputs* #:port #:emit #:try-emit #:finish #:do-input
   #:stage-ports #:stage-port-types #:extra-ports #:port-type
   ;; pipelines
   #:pipeline #:run #:collect-pipeline #:each #:join #:cancel
   #:pipeline-failures #:pipeline-threads #:pipeline-stages
   #:pipeline-channels #:pipeline-branches #:pipeline-sink #:pipeline-error #:pipeline-error-stage #:pipeline-error-cause
   #:check-pipeline #:pipeline-type-error #:pipeline-type-error-port
   #:pipeline-type-error-upstream #:pipeline-type-error-downstream
   ;; built-in stages
   #:from-list #:counter #:ls #:lines #:sh #:to-sh #:glob #:ps
   #:glob-match #:glob-pattern-p #:read-directory-names #:basename #:map-glob
   #:where #:xform #:take #:drop #:uniq #:sort-by #:tally #:accumulate
   #:peek #:to-text #:print-items #:table #:to-file #:from-file #:tee #:route
   ;; external processes
   #:command-failed #:command-failed-command #:command-failed-exit-code
   #:command-failed-stderr #:emit-lines
   ;; help -- the registry's accessors stay internal; (help NAME) is the API
   #:help #:explain #:*stages* #:stage-info
   ;; live view (src/watch.lisp)
   #:watch #:watch-pipeline #:*watch-interval*
   ;; word-mode reader (src/reader.lisp)
   #:read-shell #:shell-syntax-p #:shell-tokens
   ;; presentation (src/present.lisp)
   #:present #:render-table #:table-columns #:with-output-lock #:*output-lock*
   #:*before-output*
   ;; terminal colour (src/ansi.lisp)
   #:+esc+ #:*color* #:color-p #:paint #:visible-width #:terminal-width
   ;; misc
   ;; COPY-* are the escape hatch TEE documents: copying is a stage.
   #:file-entry #:make-file-entry #:copy-file-entry #:file-entry-path #:file-entry-name
   #:file-entry-size #:file-entry-mtime #:file-entry-dir-p #:file-entry-type
   #:file-entry-mode #:file-entry-nlink #:file-entry-uid #:file-entry-gid
   #:file-entry-user #:file-entry-group #:file-entry-ino #:file-entry-atime
   #:file-entry-ctime #:file-entry-target #:mode-string #:file-type-of
   #:file-entry-dev #:file-entry-birthtime #:file-entry-blocks #:file-entry-blksize
   #:file-entry-mtime-nsec #:file-entry-atime-nsec #:file-entry-ctime-nsec
   ;; one lstat, with what sb-posix does not surface (src/stat.lisp)
   #:file-stat #:make-file-stat #:precise-time
   #:+unix-to-universal+ #:universal-from-unix
   #:fs-size #:fs-mode #:fs-nlink #:fs-uid #:fs-gid #:fs-ino #:fs-dev #:fs-rdev
   #:fs-atime #:fs-atime-nsec #:fs-mtime #:fs-mtime-nsec #:fs-ctime #:fs-ctime-nsec
   #:fs-birthtime #:fs-blocks #:fs-blksize
   #:line #:make-line #:copy-line #:line-text #:line-number #:line-source
   #:process #:make-process #:copy-process #:process-pid #:process-ppid
   #:process-user #:process-state #:process-pcpu #:process-pmem
   #:process-rss #:process-vsz #:process-etime #:process-tty
   #:process-name #:process-command #:process-args))

;;; Loaded only by the PLUMB/CLI system, but declared here so that every
;;; package in the project has one home.

(defpackage #:plumb.lineedit
  (:use #:cl)
  (:nicknames #:ple)
  (:documentation "Emacs-key line editing for the REPL.  See src/lineedit.lisp.")
  ;; Colour lives in the core; re-exported here so PLE:PAINT and PLUMB:PAINT
  ;; are the same symbol rather than two implementations.
  (:import-from #:plumb #:+esc+ #:*color* #:color-p #:paint #:visible-width
                #:terminal-width)
  (:export
   #:read-line-edited #:tty-p
   #:*prompt* #:*continuation-prompt* #:prompt-text
   #:*history* #:*history-limit* #:add-history
   #:*history-file* #:load-history #:append-history
   #:*completer* #:complete
   #:*color* #:color-p #:paint #:visible-width #:terminal-width))

(defpackage #:plumb.cli
  (:use #:cl)
  (:documentation "The `plumb` executable.  See src/cli.lisp.")
  (:export #:main #:repl #:*version* #:*prompt-directory-width*))
