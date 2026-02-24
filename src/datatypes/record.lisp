(in-package :centi)

(defstruct (record (:predicate record?))
  type
  data)

(defun record (type &rest data)
  (make-record :type type
               :data (coerce data 'vector)))

(defun record-length (r)
  (length (record-data r)))

(defun record-get (r index)
  (aref (record-data r) index))

(defun record-set! (r index value)
  (setf (aref (record-data r) index) value)
  r)

(defmethod print-object ((object record) stream)
  (format stream "#<~a>" (aref (record-type object) 0)))
