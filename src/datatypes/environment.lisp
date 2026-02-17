(in-package :centi)

(defstruct (environment (:predicate environment?))
  parent
  bindings)

(defmethod print-object ((object environment) stream)
  (print-unreadable-object (object stream :identity t :type t)))

(defun environment (&optional (parent t present?))
  (make-environment :parent (if present? parent (intern "nil"))
                    :bindings (make-hash-table)))

(defun environment-get (e binding)
  (gethash binding (environment-bindings e)))

(defun environment-set! (e binding value)
  (setf (gethash binding (environment-bindings e)) value)
  e)

(defun environment-in? (e binding)
  (multiple-value-bind (value present?)
      (environment-get e binding)
    (declare (ignore value))
    present?))

(defun environment-empty? (e)
  "Check if environment is empty."
  (eq e (intern "nil")))

(defun environment-find (e binding)
  (unless (environment-empty? e)
    (if (environment-in? e binding)
        e
        (environment-find (environment-parent e) binding))))
