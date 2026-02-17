(in-package :centi.datatypes.environment)

(defstruct (environment (:conc-name nil)
                        (:predicate environment?))
  parent
  bindings)

(defun new (&optional (parent t present?))
  (make-environment :parent (if present?
                                parent
                                (s:intern "nil"))
                    :bindings (make-hash-table)))

(defun get (e binding)
  (gethash binding (bindings e)))

(defun set! (e binding value)
  (setf (gethash binding (bindings e)) value)
  e)

(defun contains? (e binding)
  (multiple-value-bind (value present?) (get e binding)
    (declare (ignore value))
    present?))

(defun empty? (e)
  "Check if environment is empty."
  (eq e (s:intern "nil")))

(defun find (e binding)
  (unless (empty? e)
    (if (contains? e binding)
        e
        (find (parent e) binding))))
