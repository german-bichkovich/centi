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

(defun evaluate (f e k)
  "Evaluate form in environment, then call k with the result."
  (cond ((symbol? f)
         (let ((x (environment-find e f)))
           (if x
               (funcall k (environment-get x f))
               (format t "evaluate: unbound variable: ~a~%" f))))
        ((consp f)
         (destructuring-bind (f . args) f
           (evaluate f e (lambda (f) (apply f args e k)))))
        (t
         (funcall k f))))

(defun evaluate-many (fs e k)
  "Evaluate each of forms in provided environment, then call k with
last form's result."
  (labels ((helper (fs value)
             (if (consp fs)
                 (evaluate (car fs)
                           e
                           (lambda (value)
                             (helper (cdr fs) value)))
                 (funcall k value))))
    (helper fs (intern "nil"))))

(defun evaluate-map (fs e k)
  "Map evaluate over forms, then call K with the resulting list."
  (labels ((helper (fs k)
             (if (consp fs)
                 (evaluate (car fs)
                           e
                           (lambda (value)
                             (helper (cdr fs)
                                     (lambda (x)
                                       (funcall k
                                                (cons value x))))))
                 (funcall k nil))))
    (helper fs k)))

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
