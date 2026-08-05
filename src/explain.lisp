;;;; explain.lisp -- what a pipeline is, without running it.
;;;;
;;;; Constructing a stage spawns nothing -- RUN does that -- so a pipeline can
;;;; be taken apart and drawn while it is still inert.  Everything shown here
;;;; is recorded metadata: the type signature and ports from DEFSTAGE, the
;;;; argument values the constructor was called with, and the channel depth
;;;; RUN would use.
;;;;
;;;; Drawing an *invalid* pipeline is the point, not an edge case.  CHECK-PIPELINE
;;;; refuses to run one and says which pair disagreed; EXPLAIN shows the whole
;;;; shape with the bad joint marked in place, which is what you want when the
;;;; mismatch is four stages in.

(in-package #:plumb)

(defun explain-value (value)
  "A value as it should appear in a diagram: short, and never several lines."
  (let ((*print-pretty* nil) (*print-length* 4) (*print-level* 2))
    (typecase value
      (function "fn")
      (stream "stream")
      (string (prin1-to-string value))
      (t (let ((text (let ((*print-case* :downcase)) (prin1-to-string value))))
           (if (> (length text) 24)
               (concatenate 'string (subseq text 0 23) "…")
               text))))))

(defun explain-arguments (stage)
  "The arguments this stage was actually built with.  DEFSTAGE records them, so
this is what the caller wrote rather than what the lambda list allows.  Keys
whose value is NIL are defaults nobody asked for, and would only be noise."
  (let ((parts '()))
    (loop for (key value) on (stage-args stage) by #'cddr
          when value
            do (push (format nil "~(~a~)=~a" key (explain-value value)) parts))
    (nreverse parts)))

(defun explain-signature (stage)
  (format nil "~a → ~a"
          (render-type (stage-consumes stage))
          (render-type (stage-produces stage))))

(defun explain-kind (stage)
  (cond ((null (stage-consumes stage)) "source")
        ((null (stage-produces stage)) "sink")
        (t "transform")))

(defun explain (stages &optional (stream *standard-output*))
  "Draw STAGES without running them: what each stage is, what it carries, where
the channels sit, and whether the types line up.  Returns no values."
  (let* ((stages (remove nil (if (stage-p stages) (list stages) stages)))
         (n (length stages)))
    (unless stages
      (format stream "~&Nothing to explain.~%")
      (return-from explain (values)))
    (let* ((labels (mapcar (lambda (s)
                             (format nil "~(~a~)~@[ ~{~a~^ ~}~]"
                                     (stage-name s) (explain-arguments s)))
                           stages))
           (width (reduce #'max labels :key #'length :initial-value 0))
           (problems '()))
      (format stream "~&~a~%~%"
              (paint (format nil "pipeline of ~d stage~:p, ~d channel~:p, ~d thread~:p"
                             n (1- n) n)
                     :bold))
      (loop for stage in stages
            for label in labels
            for i from 0
            for next = (nth (1+ i) stages)
            do (format stream "  ~va   ~a  ~a~%"
                       width (paint label :cyan)
                       (paint (format nil "~9a" (explain-kind stage)) :grey)
                       (paint (explain-signature stage) :grey))
               ;; Between two stages sits one bounded channel; after the last
               ;; one sits whatever RUN was handed as a sink.
               (when (stage-barrier stage)
                 (format stream "  ~a~%"
                         (paint "    ⋯ barrier: emits nothing until its input ends"
                                :yellow)))
               (cond
                 ;; A sink has no :out worth drawing; anything else feeds the
                 ;; channel RUN was handed as a sink.
                 ((and (null next) (null (stage-produces stage))))
                 ((null next)
                  (format stream "  ~a~%" (paint "↓ sink" :grey)))
                 ((%compatible-p (stage-produces stage) (stage-consumes next))
                  (format stream "  ~a~%"
                          (paint (format nil "│ channel, capacity ~d" *default-capacity*)
                                 :grey)))
                 (t
                  (push (cons stage next) problems)
                  (format stream "  ~a~%"
                          (paint (format nil "✗ ~a produces ~a but ~(~a~) consumes ~a"
                                         (string-downcase (stage-name stage))
                                         (render-type (stage-produces stage))
                                         (stage-name next)
                                         (render-type (stage-consumes next)))
                                 :red :bold)))))
      (terpri stream)
      (if problems
          (format stream "~a~%"
                  (paint (format nil "~d type error~:p -- RUN would refuse this pipeline."
                                 (length problems))
                         :red :bold))
          (format stream "~a~%" (paint "types check; RUN would start it." :green)))))
  (values))
