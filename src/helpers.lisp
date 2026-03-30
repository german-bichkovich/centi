(in-package :centi)

(defparameter *stdenv* nil
  "Standard environment that includes all builtins.")

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

(defun define (name thing)
  (environment-set! *stdenv* (intern name) thing))

(defun fold (f value list)
  (if (consp list)
      (fold f
            (funcall f value (car list))
            (cdr list))
    value))

(defun fold-right (f value list)
  (if (consp list)
      (funcall f
               (fold-right f
                           value
                           (cdr list))
               (car list))
      value))
