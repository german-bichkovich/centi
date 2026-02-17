(in-package :centi.datatypes.symbol)

(defstruct (symbol (:conc-name nil)
                   (:copier nil)
                   (:predicate symbol?)
                   (:constructor new (name)))
  name)

(defmethod print-object ((object symbol) stream)
  (format stream "~a" (name object)))

(defvar *intern-table*
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "nil" h) (new "nil"))
    h))

(defun intern (name)
  (multiple-value-bind (value present?)
      (gethash name *intern-table*)
    (if present?
        value
        (setf (gethash name *intern-table*)
              (new name)))))

(defun interned? (name)
  (multiple-value-bind (value present?)
      (gethash name *intern-table*)
    (declare (ignore value))
    present?))
