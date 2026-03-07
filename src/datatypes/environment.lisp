(in-package :centi)

(defstruct (environment (:predicate environment?))
  bindings
  parent)

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

(defun environment-find (e binding)
  (when (environment? e)
    (if (environment-in? e binding)
        e
        (environment-find (environment-parent e) binding))))
