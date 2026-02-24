(in-package :centi)

(defstruct (symbol (:copier nil)
                   (:predicate symbol?)
                   (:constructor symbol (%name)))
  %name)

(defun symbol-name (symbol)
  (if symbol
      (symbol-%name symbol)
      "()"))

(defmethod print-object ((object symbol) stream)
  (loop for c across (symbol-name object) do
    (princ (case c
             (#\return "\\r")
             (#\escape "\\e")
             (#\newline "\\n")
             (#\tab "\\t")
             (#\space "\\ ")
             (#\( "\\(")
             (#\) "\\)")
             (#\[ "\\[")
             (#\] "\\]")
             (#\{ "\\{")
             (#\} "\\}")
             (t c))
           stream)))

(defvar *intern-table*
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "nil" h) (symbol "nil"))
    h))

(defun intern (name)
  (multiple-value-bind (value present?)
      (gethash name *intern-table*)
    (if present?
        value
        (setf (gethash name *intern-table*)
              (symbol name)))))

(defun interned? (symbol)
  (multiple-value-bind (value present?)
      (gethash symbol *intern-table*)
    (declare (ignore value))
    present?))
