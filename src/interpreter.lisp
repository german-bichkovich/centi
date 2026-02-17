(in-package :centi.interpreter)

(defun destructure (ptree form callback)
  "Destructure FORM over PTREE. Whenever ptree is a symbol, do
(callback symbol form)."
  (cond ((null ptree)
         (when form
           (error "destructure: too many arguments")))
        ((s:symbol? ptree)
         (unless (eq (s:intern "nil") ptree)
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
  (cond ((s:symbol? form)
         (let ((x (e:find env form)))
           (if x
               (e:get x form)
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
  (cond ((c:special? form)
         (with-slots (c:ptree c:body c:environment c:ebind) form
           (if (functionp c:body)
               (funcall c:body args env)
               (let ((new-env (e:new c:environment)))
                 (destructure c:ptree
                              args
                              (lambda (k v) (e:set! new-env k v)))
                 (unless (eq c:ebind (s:intern "nil"))
                   (e:set! new-env c:ebind env))
                 (eval-many c:body new-env)))))
        ((c:function? form)
         (apply (c:unwrap form)
                (mapcar (lambda (form) (eval form env)) args)
                env))
        ((c:generic? form)
         (apply (eval (s:intern "generic:call") env)
                (cons form args)
                env))
        (t
         (error "capply: can't apply ~a" form))))
