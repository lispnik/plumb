;;;; package.lisp

(defpackage #:plumb
  (:use #:cl)
  (:nicknames #:pb)
  (:export
   ;; channels
   #:channel #:make-channel #:channel-p
   #:send #:recv #:close-output #:close-input
   #:channel-closed #:channel-closed-channel
   #:*default-capacity*
   ;; fields
   #:field #:fields #:$ #:fld
   ;; stages
   #:stage #:stage-p #:stage-name #:stage-args #:stage-consumes #:stage-produces #:stage-barrier
   #:defstage
   #:*input* #:*outputs* #:port #:emit #:finish #:do-input
   ;; pipelines
   #:pipeline #:run #:collect-pipeline #:each #:join #:cancel
   #:pipeline-failures #:pipeline-threads #:pipeline-stages
   #:pipeline-channels #:pipeline-sink #:pipeline-error #:check-pipeline #:pipeline-type-error
   ;; built-in stages
   #:from-list #:counter #:ls #:lines #:sh #:to-sh #:glob
   #:where #:xform #:take #:drop #:uniq #:sort-by #:tally #:accumulate
   #:peek #:to-text #:print-items #:table #:to-file #:from-file #:tee
   ;; external processes
   #:command-failed #:command-failed-command #:command-failed-exit-code
   #:command-failed-stderr #:emit-lines
   ;; help -- the registry's accessors stay internal; (help NAME) is the API
   #:help #:explain #:*stages* #:stage-info
   ;; word-mode reader (src/reader.lisp)
   #:read-shell #:shell-syntax-p #:shell-tokens
   ;; presentation (src/present.lisp)
   #:present #:render-table #:table-columns
   ;; terminal colour (src/ansi.lisp)
   #:+esc+ #:*color* #:color-p #:paint #:visible-width
   ;; misc
   ;; COPY-* are the escape hatch TEE documents: copying is a stage.
   #:file-entry #:make-file-entry #:copy-file-entry #:file-entry-path #:file-entry-name
   #:file-entry-size #:file-entry-mtime #:file-entry-dir-p
   #:line #:make-line #:copy-line #:line-text #:line-number #:line-source))

;;; Loaded only by the PLUMB/CLI system, but declared here so that every
;;; package in the project has one home.

(defpackage #:plumb.lineedit
  (:use #:cl)
  (:nicknames #:ple)
  (:documentation "Emacs-key line editing for the REPL.  See src/lineedit.lisp.")
  ;; Colour lives in the core; re-exported here so PLE:PAINT and PLUMB:PAINT
  ;; are the same symbol rather than two implementations.
  (:import-from #:plumb #:+esc+ #:*color* #:color-p #:paint #:visible-width)
  (:export
   #:read-line-edited #:tty-p
   #:*prompt* #:*continuation-prompt* #:prompt-text
   #:*history* #:*history-limit* #:add-history
   #:*history-file* #:load-history #:append-history
   #:*completer* #:complete
   #:*color* #:color-p #:paint #:visible-width))

(defpackage #:plumb.cli
  (:use #:cl)
  (:documentation "The `plumb` executable.  See src/cli.lisp.")
  (:export #:main #:repl #:*version* #:*prompt-directory-width*))
