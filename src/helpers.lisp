(defun bool->cbool (x)
  "Convert CL boolean to Centi boolean."
  (if x (intern "true") (intern "false")))

(defun cbool->bool (x)
  "Convert Centi boolean to CL boolean."
  (not (member x (list (intern "false")
                       (intern "nil")))))

(defun array? (object)
  (and (arrayp object)
       (eq t (array-element-type object))))

(defun positive-integer? (object)
  (and (integerp object)
       (<= 0 object)))

(defun byte? (object)
  (and (integerp object)
       (<= 0 object 255)))

(defmacro define (name thing)
  `(progn (environment-set! *stdenv* (intern ,name) ,thing)
          nil))

(defmacro special (ptree ebind &rest body)
  (declare (ignorable ptree ebind))
  `(special-new
    :ebind ,(if (consp ebind) ebind `',ebind)
    :ptree 'args
    :environment 'nil
    :body (lambda (args env k)
            (declare (ignorable args env k))
            (block nil
              ,@body))))

(defmacro function (parameter &rest body)
  `(wrap (special ,parameter (centi:intern "nil") . ,body)))
