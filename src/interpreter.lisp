(in-package :centi)

;; TODO cps
(defun destructure (ptree form callback)
  "Destructure FORM over PTREE. Whenever ptree is a symbol, do
(callback symbol form)."
  (cond ((null ptree)
         (when form
           (error "destructure: too many arguments")))
        ((symbol? ptree)
         (unless (eq (intern "nil") ptree)
           (funcall callback ptree form)))
        ((consp ptree)
         (cond ((consp form)
                (destructure (car ptree) (car form) callback)
                (destructure (cdr ptree) (cdr form) callback))
               ((not form)
                (error "destructure: too few arguments"))
               (t
                (error "destructure: not a pair: ~a" form))))
        (t
         (error "destructure: unsupported ptree: ~a" ptree))))

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

(defun evaluate-many (forms environment k)
  "Evaluate each of forms in provided environment, then call k with
last form's result."
  (funcall (fold-right (lambda (k f)
                         (lambda (_)
                           (declare (ignore _))
                           (evaluate f environment k)))
                       k
                       forms)
           (intern "nil")))

(defun evaluate-map (forms environment k)
  "Map evaluate over forms, then call K with the resulting list."
  (funcall (fold (lambda (k form)
                   (lambda (values)
                     (evaluate form
                               environment
                               (lambda (value)
                                 (funcall k (cons value values))))))
                 k
                 forms)
           nil))

(defun evaluate (f e k)
  "Evaluate form in environment, then call k with the result."
  (cond ((symbol? f)
         (let ((x (environment-find e f)))
           (if x
               (funcall k (environment-get x f))
               (error "evaluate: unbound variable: ~a~%" f))))
        ((consp f)
         (destructuring-bind (f . args) f
           (evaluate f e (lambda (f) (apply f args e k)))))
        (t
         (funcall k f))))

(defun apply (f a e k)
  (cond ((special? f)
         (with-slots (ptree body environment ebind) f
           (if (functionp body)
               (funcall body a e k)
               (let ((new-env (environment environment)))
                 (destructure ptree
                              a
                              (lambda (k v)
                                (environment-set! new-env k v)))
                 (unless (eq ebind (intern "nil"))
                   (environment-set! new-env ebind e))
                 (evaluate-many body new-env k)))))
        ((function? f)
         (evaluate-map a e (lambda (a)
                             (apply (unwrap f) a e k))))
        (t
         (error "capply: can't apply ~a" f))))

;; TODO move elsewhere
(defun error (&rest args)
  (cl:apply #'format t args)
  (intern "nil"))

(defmacro assert (test &rest format-args)
  `(if ,test
       (intern "nil")
       (return (centi::error . ,format-args))))
