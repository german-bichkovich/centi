(in-package :centi.datatypes.record)

(defstruct (record
            (:predicate record?))
  data)

(defun record (&rest data)
  (make-record :data (coerce data 'vector)))

(defun length (r)
  (length (record-data r)))

(defun get (r index)
  (aref (record-data r) index))

(defun set! (r index value)
  (setf (aref (record-data r) index) value)
  r)

(defmethod print-object ((object record) stream)
  (format stream "#<~a>" (get object 0)))
