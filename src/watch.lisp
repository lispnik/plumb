;;;; watch.lisp -- what a pipeline is *doing*, while it does it.
;;;;
;;;; EXPLAIN draws a pipeline before it runs; this draws the same shape while
;;;; it runs, with the numbers only a live pipeline has.  The layout is
;;;; deliberately EXPLAIN's -- a line per stage, a `│` connector per channel --
;;;; so the two read as one tool rather than two that both happen to draw
;;;; pipelines.
;;;;
;;;; Everything shown comes off the channels, which is why watching costs
;;;; almost nothing:
;;;;
;;;;   CHANNEL-COUNT / CHANNEL-CAPACITY   occupancy -- where the backpressure is
;;;;   CHANNEL-PASSED                     cumulative throughput
;;;;   CHANNEL-LAST                       the most recent object
;;;;
;;;; A full channel in front of a stage that has emitted nothing is the whole
;;;; point: that is the bottleneck, named, without a profiler.
;;;;
;;;; Two entry points, one renderer:
;;;;
;;;;   watch ls | where {...} | tally     the whole pipeline (WATCH-PIPELINE)
;;;;   ls | watch | where {...} | tally   one point           (the WATCH stage)
;;;;
;;;; Both register into one registry and one watcher thread draws whatever is
;;;; registered, so the two forms cannot drift apart.
;;;;
;;;; The panel goes to *ERROR-OUTPUT*, so stdout stays pipeable -- `watch ls |
;;;; take 5` still counts 5 through `wc -l`, and 2>/dev/null removes the panel
;;;; without touching the data.

(in-package #:plumb)

(defvar *watch-interval* 0.2
  "Seconds between repaints.  Also the window the rate is measured over.")

(defstruct (watch-point (:conc-name wp-) (:copier nil))
  label                                 ; the stage line
  detail                                ; its arguments, and any barrier note
  channel                               ; whose counters this reads
  (previous 0)                          ; PASSED at the last repaint
  (previous-time 0)                     ; and when, for the rate
  (rate 0))

;;; ---------------------------------------------------------------- registry

(defvar *watch-points* '()
  "Registered points, in pipeline order.  Both entry points push here and the
one watcher thread draws whatever it finds.")
(defvar *watch-lock* (sb-thread:make-mutex :name "plumb-watch"))
(defvar *watchers* 0
  "How many WATCHes are running.  Refcounted for the same reason CLOSE-OUTPUT
is: two WATCH stages in one pipeline must not have the first to finish stop
the panel for the second.")
(defvar *watcher-thread* nil)
(defvar *watch-stream* nil)
(defvar *panel-height* 0
  "Lines the live panel currently occupies, so it can be erased exactly.")
(defvar *panel-live-p* nil
  "Is the panel being repainted in place?  Only when its stream is a terminal;
a redirected stderr gets one static block at the end instead of a screenful of
cursor motion.")

(defun watch-points ()
  (sb-thread:with-mutex (*watch-lock*) (copy-list *watch-points*)))

(defun register-watch-points (points)
  (sb-thread:with-mutex (*watch-lock*)
    (setf *watch-points* (append *watch-points* points)))
  points)

(defun unregister-watch-points (points)
  (sb-thread:with-mutex (*watch-lock*)
    (setf *watch-points* (remove-if (lambda (p) (member p points)) *watch-points*)))
  (values))

;;; ----------------------------------------------------------------- drawing

(defun watch-paint (string &rest styles)
  "PAINT decides from *STANDARD-OUTPUT*, and the panel goes to stderr.  Same
rebinding REPORT does in cli.lisp, for the same reason: a redirected panel
should be plain text even when stdout is a terminal."
  (let ((*standard-output* (or *watch-stream* *error-output*)))
    (apply #'paint string styles)))

(defun truncate-to (string width)
  (if (> (length string) width)
      (concatenate 'string (subseq string 0 (max 1 (1- width))) "…")
      string))

(defun rate-text (rate)
  (cond ((null rate) "")
        ((>= rate 1000000) (format nil "~,1fM/s" (/ rate 1000000)))
        ((>= rate 1000) (format nil "~,1fk/s" (/ rate 1000)))
        ((>= rate 1) (format nil "~d/s" (round rate)))
        ((plusp rate) (format nil "~,2f/s" rate))
        (t "")))

(defun occupancy-bar (count capacity &optional (cells 10))
  "A channel's fullness, drawn.  The number alone is readable but the bar is
what makes one full channel in a column of empty ones jump out."
  (let ((filled (if (plusp capacity)
                    (min cells (round (* cells (/ count capacity))))
                    0)))
    (concatenate 'string
                 (make-string filled :initial-element #\█)
                 (make-string (- cells filled) :initial-element #\·))))

(defun update-rate (point)
  "Objects per second since the last repaint, measured against the clock rather
than assumed to be the interval -- a repaint delayed by a slow terminal would
otherwise report a rate that never happened."
  (let* ((now (get-internal-real-time))
         (passed (channel-passed (wp-channel point)))
         (elapsed (/ (- now (wp-previous-time point))
                     internal-time-units-per-second)))
    (when (plusp elapsed)
      (setf (wp-rate point) (/ (- passed (wp-previous point)) elapsed)))
    (setf (wp-previous point) passed
          (wp-previous-time point) now)
    (wp-rate point)))

(defun stage-line (point label-width width)
  "Stage name and arguments on the left, throughput on the right.

Everything is sized and truncated while still plain, and painted only at the
end.  Truncating afterwards counts SGR escapes as visible columns and eats the
right-hand column -- which is the one worth reading.  Same trap as PAD BEFORE
PAINTING in render-table, arrived at from the other direction."
  (let* ((passed (channel-passed (wp-channel point)))
         (rate (update-rate point))
         (right (format nil "~:d obj~:p  ~a" passed (rate-text rate)))
         ;; The right column is never sacrificed: the label gives up room first.
         (room (max 8 (- width (length right) 2)))
         (left (truncate-to (format nil "  ~va  ~@[~a~]" label-width
                                    (wp-label point) (wp-detail point))
                            room))
         (gap (max 2 (- width (length left) (length right)))))
    (concatenate 'string
                 (watch-paint left :bold)
                 (make-string gap :initial-element #\Space)
                 (watch-paint right :grey))))

(defun channel-line (point width)
  (let* ((ch (wp-channel point))
         (count (channel-count ch))
         (capacity (channel-capacity ch))
         (last (channel-last ch))
         (text (if (channel-discard ch)
                   ;; A discard sink never buffers, so an occupancy of 0/64
                   ;; would be a fact about nothing.
                   "  │ discarded"
                   (format nil "  │ ~a ~d/~d" (occupancy-bar count capacity)
                           count capacity))))
    (when last
      (setf text (format nil "~a  last: ~a" text
                         (truncate-to (present last) (max 8 (- width (length text) 10))))))
    (watch-paint (truncate-to text width) :grey)))

(defun panel-lines ()
  (let* ((points (watch-points))
         (width (terminal-width))
         (label-width (reduce #'max points :key (lambda (p) (length (wp-label p)))
                                           :initial-value 0)))
    (when points
      (cons (watch-paint (format nil "watching ~d stage~:p" (length points)) :bold)
            (loop for p in points
                  append (list (stage-line p label-width width)
                               (channel-line p width)))))))

(defun erase-panel ()
  "Take the live panel down: up N lines, then clear to the end of the screen.
Called from *BEFORE-OUTPUT* as well as from the watcher, so anything writing to
a shared stream scrolls into clean ground rather than through the panel."
  (when (and *panel-live-p* (plusp *panel-height*) *watch-stream*)
    (format *watch-stream* "~c[~dA~c[J" +esc+ *panel-height* +esc+)
    (force-output *watch-stream*))
  (setf *panel-height* 0)
  (values))

(defun draw-panel (&key (live t))
  (with-output-lock                     ; the hook erases the previous panel
    (let ((lines (panel-lines))
          (stream (or *watch-stream* *error-output*)))
      (dolist (line lines) (write-line line stream))
      (force-output stream)
      (setf *panel-height* (if live (length lines) 0))))
  (values))

;;; ------------------------------------------------------------- the watcher

(defun watcher-loop (interval)
  ;; A rendering error must not take the panel down without saying so: this
  ;; runs in its own thread, where an unhandled condition is invisible until
  ;; someone joins it, and the symptom is a watch that quietly does nothing.
  (handler-case
      (loop while (plusp *watchers*)
            do (sleep interval)
               (when (plusp *watchers*) (draw-panel)))
    (error (c)
      (setf *panel-live-p* nil *before-output* nil)
      (ignore-errors
       (format (or *watch-stream* *error-output*) "~&watch: panel stopped: ~a~%" c)
       (force-output (or *watch-stream* *error-output*))))))

(defun start-watching (&key (interval *watch-interval*) (stream *error-output*))
  "Begin repainting.  Refcounted: the second caller joins the first's watcher."
  (sb-thread:with-mutex (*watch-lock*)
    (incf *watchers*)
    (when (= 1 *watchers*)
      (setf *watch-stream* stream
            *panel-height* 0
            *panel-live-p* (and (interactive-stream-p stream) t)
            *before-output* (when *panel-live-p* #'erase-panel)
            *watcher-thread*
            (sb-thread:make-thread (lambda () (watcher-loop interval))
                                   :name "plumb:watch"))))
  (values))

(defun stop-watching ()
  "Stop repainting and leave the totals behind.  The final panel is drawn
statically -- it is the residue worth keeping, and on a redirected stderr it is
the only thing drawn at all."
  (let ((last nil))
    (sb-thread:with-mutex (*watch-lock*)
      (when (plusp *watchers*) (decf *watchers*))
      (setf last (zerop *watchers*)))
    (when last
      (let ((thread *watcher-thread*))
        (setf *watcher-thread* nil)
        (when thread (ignore-errors (sb-thread:join-thread thread :default nil))))
      (with-output-lock (erase-panel))
      (setf *before-output* nil)
      (draw-panel :live nil)
      (setf *panel-live-p* nil)))
  (values))

;;; ------------------------------------------------------- the pipeline form

(defun tap-watch-point (label channel)
  (make-watch-point :label label :detail nil :channel channel
                    :previous (channel-passed channel)
                    :previous-time (get-internal-real-time)))

(defun stage-watch-point (stage channel)
  (make-watch-point
   :label (string-downcase (symbol-name (stage-name stage)))
   :detail (let ((args (explain-arguments stage)))
             (format nil "~{~a~^ ~}~:[~; ⋯ barrier~]"
                     args (stage-barrier stage)))
   :channel channel
   :previous (channel-passed channel)
   :previous-time (get-internal-real-time)))

(defun watch-pipeline (stages &key (interval *watch-interval*)
                                   (stream *error-output*))
  "Run STAGES with a live panel on STREAM.  This is what the word `watch`
expands to at the head of a pipeline, the way `explain` does:

  watch ls \"**/*\" | sort-by .size | take 10

Each stage is watched through the channel it emits into, so its counter is the
number of objects it has produced.  Objects are printed as they arrive, exactly
as the CLI prints them without WATCH -- the panel is on stderr and takes itself
down before anything else writes, so `watch ... | wc -l` is still the count and
`2>/dev/null` still gets clean data.

Draining happens in this thread, so backpressure reaches the source; that is
what makes the occupancy numbers real rather than an artefact of watching."
  (setf stages (remove nil stages))
  (assert stages () "Empty pipeline.")
  (let* ((sink (make-channel :capacity *default-capacity* :name "watch"))
         (pipe (run stages :sink sink))
         (points (loop for stage in (pipeline-stages pipe)
                       for channel in (append (pipeline-channels pipe) (list sink))
                       collect (stage-watch-point stage channel))))
    (register-watch-points points)
    (start-watching :interval interval :stream stream)
    (unwind-protect
         (loop (multiple-value-bind (object ok) (recv sink)
                 (unless ok (return))
                 (with-output-lock
                   (write-line (present object))
                   (force-output))))
      (close-input sink)
      (stop-watching)
      (unregister-watch-points points)
      (join pipe :errorp t)))
  (values))

;;; ----------------------------------------------------------- the tap form

(defstage watch (&key (label "watch") (interval *watch-interval*)
                      (stream *error-output*))
  "Tap: watch this one point and pass every object through unchanged.

  ls \"**/*\" | watch | where {(> .size 1mb)} | tally

Where WATCH-PIPELINE draws every stage, this draws one -- useful when only one
joint is in question, and the only form available in the middle of a pipeline
built from Lisp.  What it reports is its own output channel, so the count is
objects that have gone past.

Like PEEK, and unlike a sink: the stream is untouched."
  (:consumes :objects) (:produces :objects)
  (let ((point (tap-watch-point label (port :out))))
    (register-watch-points (list point))
    (start-watching :interval interval :stream stream)
    (unwind-protect
         (do-input (x) (emit x))
      (stop-watching)
      (unregister-watch-points (list point)))))
