(in-package :centi)

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

(defun eval (form env)
  (cond ((symbol? form)
         (let ((x (environment-find env form)))
           (if x
               (environment-get x form)
               (error "eval: unbound variable: '~a'" form))))
        ((consp form)
         (destructuring-bind (f . args) form
           (apply (eval f env) args env)))
        (t form)))

(defun eval-many (forms env)
  "Evaluate FORMS in ENV in a loop and return last result."
  (loop for form in forms
        for result = (eval form env)
        finally (return result)))

(defun apply (form args env)
  (cond ((special? form)
         (with-slots (ptree body environment ebind) form
           (if (functionp body)
               (funcall body args env)
               (let ((new-env (environment environment)))
                 (destructure ptree
                              args
                              (lambda (k v)
                                (environment-set! new-env k v)))
                 (unless (eq ebind (intern "nil"))
                   (environment-set! new-env ebind env))
                 (eval-many body new-env)))))
        ((function? form)
         (apply (unwrap form)
                (mapcar (lambda (form) (eval form env)) args)
                env))
        ((generic? form)
         (apply (eval (intern "generic:call") env)
                (cons form args)
                env))
        (t
         (error "capply: can't apply ~a" form))))
