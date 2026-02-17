(in-package :centi.datatypes.callable)

(defstruct (special (:conc-name nil)
                    (:constructor make-special)
                    (:predicate special?))
  "A centi special form. No different from a function except that
it doesn't evaluate it's arguments."
  body
  environment
  ptree
  ebind)

(defmethod print-object ((object special) stream)
  (format stream "#<special ~a>" (ptree object)))

(defstruct (function
            (:constructor wrap (special))
            (:predicate function?))
  "A centi function type. It's just a wrapper around a special form
that forces evaluation of its arguments."
  special)

(defun unwrap (f)
  (function-special f))

(defmethod print-object ((object function) stream)
  (format stream "#<function ~a>" (ptree (unwrap object))))

(defun generic? (object)
  "Check of object is a generic function."
  (and (r:record? object)
       (eq (r:get object 0)
           (s:intern "generic"))))
