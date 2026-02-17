(in-package :centi)

(defstruct (special (:constructor special-new)
                    (:predicate special?))
  "A centi special form. No different from a function except that
it doesn't evaluate it's arguments."
  body
  environment
  ptree
  ebind)

(defmethod print-object ((object special) stream)
  (format stream "#<special ~a>" (special-ptree object)))

(defmacro special (ptree ebind &rest body)
  (let ((ptree (or ptree (intern "()")))
        (ebind-symbol (or ebind (gensym "EBIND-"))))
    `(special-new
      :ebind 'env
      :ptree ',ptree
      :environment 'nil
      :body (lambda (args ,ebind-symbol)
              (declare (ignorable args ,ebind-symbol))
              ,(if (symbolp ptree)
                   `(let ((,ptree args))
                      ,@body)
                   `(destructuring-bind ,ptree args
                      ,@body))))))

(defstruct (function
            (:constructor wrap (special))
            (:predicate function?))
  "A centi function type. It's just a wrapper around a special form
that forces evaluation of its arguments."
  special)

(defun unwrap (f)
  (function-special f))

(defmethod print-object ((object function) stream)
  (format stream
          "#<function ~a>"
          (special-ptree (unwrap object))))

(defun generic? (object)
  "Check of object is a generic function."
  (and (record? object)
       (eq (record-get object 0)
           (intern "generic"))))

(defmacro function (parameters &rest body)
  `(wrap (special ,parameters ignore . ,body)))

