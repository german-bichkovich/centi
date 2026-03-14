(in-package :centi)

(defstruct (special (:constructor special-new)
                    (:predicate special?))
  "A centi special form. No different from a function except that
it doesn't evaluate it's arguments."
  body
  environment
  ptree
  ebind
  metadata)

(defmethod print-object ((object special) stream)
  (let* ((metadata (special-metadata object))
         (entry (assoc (intern "name") metadata)))
    (if (consp entry)
        (format stream "#<special ~a>" (cdr entry))
        (format stream "#<special>"))))

(defstruct (function
            (:constructor wrap (special))
            (:predicate function?))
  "A centi function type. It's just a wrapper around a special form
that forces evaluation of its arguments."
  special)

(defun unwrap (f)
  (function-special f))

(defmethod print-object ((object function) stream)
  (let* ((object (unwrap object))
         (metadata (special-metadata object))
         (entry (assoc (intern "name") metadata)))
    (if (consp entry)
        (format stream "#<function ~a>" (cdr entry))
        (format stream "#<function>"))))

;;; Declarations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun has-declarations? (body)
  (and (consp (car body))
       (eq (caar body) (intern "declare"))))

(defun special-remove-declaration (body)
  (if (has-declarations? body) (cdr body) body))

;; TODO remove unneeded spaces, insert ptree if not provided
(defun normalize-docstring (string)
  string)

(defun special-extract-metadata (body)
  (if (has-declarations? body)
      (loop
        with binds = (cdar body)
        while binds
        for k = (pop binds)
        for v = (pop binds)
        collect (cond ((eq k (intern "documentation"))
                       (cons k (normalize-docstring v)))
                      (t
                       (cons k v))))
      ()))

;;; Convinience constructors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro special (ptree ebind &rest body)
  `(special-new
    :ebind ,(if (consp ebind) ebind `',ebind)
    :ptree 'args
    :environment 'nil
    :body (lambda (args env)
            (declare (ignorable args env))
            ,@body)))

(defmacro function (parameter &rest body)
  `(wrap (special ,parameter (centi:intern "nil") . ,body)))
