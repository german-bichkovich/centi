(in-package :centi.datatypes.hashmap)

(defun hashmap? (object)
  (hash-table-p object))

(defun copy (hashmap)
  (let ((h (make-hash-table)))
    (maphash (lambda (k v)
               (setf (gethash h k) v))
             hashmap)
    h))

(defun find (hashmap key)
  (destructuring-bind (value present?)
      (gethash key hashmap)
    (if present?
        (cons key value)
        (s:intern "false"))))

(defun contains? (hashmap key)
  (destructuring-bind (value present?)
      (gethash key hashmap)
    (declare (ignore value))
    present?))

(defun get (h key default)
  (multiple-value-bind (value present?) (gethash key h)
    (if present?
        value
        default)))

(defun set! (h key value)
  (setf (gethash key h) value)
  h)

(defun new (&key &allow-other-keys)
  (make-hash-table :test 'equal))

(defun each (fn hashmap)
  (maphash fn hashmap))
