(in-package :centi)

(defstruct (symbol (:copier nil)
                   (:predicate symbol?)
                   (:constructor symbol (name)))
  name)

(defmethod print-object ((object symbol) stream)
  (format stream "~a" (symbol-name object)))

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
