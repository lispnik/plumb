;;;; field.lisp -- one accessor across plists, hash tables, structs, CLOS
;;;; instances and alists.  This is what lets a shell say .size without caring
;;;; what kind of object it is looking at.

(in-package #:plumb)

(defgeneric field (object key &optional default)
  (:documentation "Read KEY out of OBJECT.  KEY is a keyword."))

(defmethod field (object key &optional default)
  (declare (ignore key))
  (if (eq default :error)
      (error "~s has no fields." object)
      default))

(defmethod field ((object hash-table) key &optional default)
  (multiple-value-bind (v found) (gethash key object)
    (cond (found v)
          ;; Allow string keys transparently.
          ((nth-value 1 (gethash (string key) object))
           (gethash (string key) object))
          (t default))))

(defmethod field ((object cons) key &optional default)
  (if (consp (car object))
      (let ((cell (assoc key object :test #'string-equal-designator)))
        (if cell (cdr cell) default))
      (getf object key default)))

(defun string-equal-designator (a b)
  (and (or (symbolp a) (stringp a)) (or (symbolp b) (stringp b))
       (string-equal (string a) (string b))))

(defun %slot-named (object key)
  "Find the slot of OBJECT whose name matches KEY, ignoring package."
  (let ((name (string key)))
    (loop for slot in (sb-mop:class-slots (class-of object))
          for slot-name = (sb-mop:slot-definition-name slot)
          when (string-equal name (symbol-name slot-name))
            do (return slot-name))))

(defmethod field ((object standard-object) key &optional default)
  (let ((slot (%slot-named object key)))
    (if (and slot (slot-boundp object slot))
        (slot-value object slot)
        default)))

(defmethod field ((object structure-object) key &optional default)
  (let ((slot (%slot-named object key)))
    (if slot (slot-value object slot) default)))

(defun fields (object)
  "The keys OBJECT responds to, for table rendering and completion."
  (typecase object
    (hash-table (loop for k being the hash-keys of object collect k))
    (cons (if (consp (car object))
              (mapcar #'car object)
              (loop for k in object by #'cddr collect k)))
    ((or standard-object structure-object)
     (loop for slot in (sb-mop:class-slots (class-of object))
           collect (intern (symbol-name (sb-mop:slot-definition-name slot))
                           :keyword)))
    (t '())))

;;; The surface syntax in a real shell reader would spell these {...} and .size.
;;; From plain Lisp they are a macro and a function.

(defmacro $ (&body body)
  "A one-argument lambda over the current item, bound to IT.
   ($ (> (fld :size) 1024))  ==  (lambda (it) (> (field it :size) 1024))"
  `(lambda (it)
     (declare (ignorable it))
     (flet ((fld (key &optional default) (field it key default)))
       (declare (ignorable #'fld))
       ,@body)))

(defun ensure-fn (designator)
  (etypecase designator
    (function designator)
    (symbol (fdefinition designator))))
