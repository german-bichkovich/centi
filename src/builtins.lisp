(in-package :centi)

;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bool->cbool (x)
  "Convert CL boolean to Centi boolean."
  (if x (intern "true") (intern "false")))

(defun cbool->bool (x)
  "Convert Centi boolean to CL boolean."
  (not (eq x (intern "false"))))

(defmacro define (name thing)
  `(progn (environment-set! *stdenv* (intern ,name) ,thing)
          nil))

(defparameter *stdenv* (environment)
  "Standard environment that includes all builtins.")

;;; Globals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; Default truth value.
;; (define "true" (intern "true"))
;;
;; ;; Canonical false value.
;; (define "false" (intern "false"))
;;
;; ;; Lack of value.
;; (define "nil" (intern "nil"))
;;
;; ;; Empty list.
;; (define "()" (intern "()"))

;;; Bootstrap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (evaluate form ?env)
;; Evaluate form in provided or current environment.
(define "evaluate"
  (wrap (special (form . rest) env
          (eval form (if (consp rest)
                         (car rest)
                         env)))))

;; (if 'test 'then 'else)
;; If test is true, evaluate then form, otherwise evaluate else form.
(define "if"
  (special (test then else) env
    (if (cbool->bool (eval test env))
        (eval then env)
        (eval else env))))

;; (symbol name)
;; Create a symbol that is not interned.
(define "symbol"
  (function (name)
    (symbol name)))

;; (symbol? object)
;; Check if object is a symbol.
(define "symbol?"
  (function (object)
    (bool->cbool (or (eq object nil)
                     (symbol? object)))))

;; (symbol:name symbol)
;; Return a string representing a symbol name.
(define "symbol:name"
  (function (symbol)
    (symbol-name symbol)))

;; (symbol:intern name)
;; Intern a symbol with provided name.
;; Two different outputs of (intern "x") are always the same objects.
(define "symbol:intern"
  (function (name)
    (intern name)))

;; (pair? object)
;; Check if object is a pair.
(define "pair?"
  (function (object)
    (bool->cbool (consp object))))

;; (pair head tail)
;; Construct a pair.
(define "pair"
  (function (head tail)
    (cons head tail)))

;; (pair:head! pair value)
;; Store value in head slot of a pair.
(define "pair:head!"
  (function (pair object)
    (rplaca pair object)
    pair))

;; (pair:tail! pair value)
;; Store value in tail slot of a pair.
(define "pair:tail!"
  (function (pair object)
    (rplacd pair object)
    pair))

;; (function? object)
;; Check if object is a function.
(define "function?"
  (function (object)
    (bool->cbool (function? object))))

;; (special? object)
;; Check if object is a special form.
(define "special?"
  (function (object)
    (bool->cbool (special? object))))

;; (special 'ptree 'ebind . 'body)
;; Create a new special form.
(define "special"
  (special (ptree ebind . body) env
    (special-new :ebind ebind
                 :ptree ptree
                 :body body
                 :environment env)))

;; (unwrap function)
;; Return a special form with same body as one function has.
(define "unwrap"
  (function (object)
    (unwrap object)))

;; (wrap special)
;; Return a function with same body as provided special has.
(define "wrap"
  (function (object)
    (wrap object)))

;; (number? object)
;; Check if object is a number.
(define "number?"
  (function (object)
    (bool->cbool (numberp object))))

;; (- number)
;; (- 1) => -1
;; (- number . numbers)
;; (- 9 6 3) => 0
;; In first form, return number negated.
;; In second form, subtract each of numbers from number.
(define "-"
  (function args
    (cl:apply #'- args)))

;; (/ divident . divisors)
;; Divide divident by each of the divisors in a sequence.
(define "/"
  (function args
    (cl:apply #'/ args)))

;; (modulo divident divisor)
;; Divide divident by divisor and return the remainder.
(define "modulo"
  (function args
    (cl:apply #'mod args)))

;; (< . numbers)
;; Check that each of the numbers is strictly smaller than next one.
(define "<"
  (function args
    (bool->cbool (cl:apply #'< args))))

;; (<< number n)
;; Bit shift number by n bits to the left.
(define "<<"
  (function (number n)
    (ash number n)))

;; (>> number n)
;; Bit shift number by n bits to the right.
(define ">>"
  (function (number n)
    (ash number (- n))))

;; (array? object)
;; Check if object is an array.
(define "array?"
  (function (object)
    (bool->cbool (and (arrayp object)
                      (eq t (array-element-type object))))))

;; (array:length array)
;; Get length of an array.
(define "array:length"
  (function (a)
    (length a)))

;; (array::new length default)
;; Primitive constructor for a general array.
(define "array::new"
  (function (length &optional (default nil default?))
    (make-array length
                :initial-element (if default?
                                     default
                                     (intern "nil"))
                :element-type t)))

;; (array:get array index)
;; (array:get array index default)
;; Get element of an array at index.
(define "array:get"
  (function (a n . rest)
    (if (< -1 n (length a))
        (aref a n)
        (if (consp rest)
            (car rest)
            (intern "nil")))))

;; (array:set! array index value)
;; Set element of an array at index.
(define "array:set!"
  (function (a n v)
    (setf (aref a n) v)
    a))

;; (string? object)
;; Check if object is a string.
(define "string?"
  (function (object)
    (bool->cbool (and (arrayp object)
                      (eq 'character (array-element-type object))))))

;; (string:length string)
;; Get length of a string.
(define "string:length"
  (function (string)
    (length string)))

;; (string::new length default)
;; A primitive constructor for a string.
(define "string::new"
  (function (length &optional (default 0))
    (make-array length
                :initial-element (code-char default)
                :element-type 'character)))

;; (string:get array index)
;; Get element of an array at index.
;; TODO get nth character vs get nth byte?
(define "string:get"
  (function (a n)
    (char-code (aref a n))))

;; (string:set! array index value)
;; Set element of an array at index.
(define "string:set!"
  (function (a n v)
    (setf (aref a n) (code-char v))
    a))

;; (string:code string)
;; Return the code encoded in first character of the string.
(define "string:code"
  (function (s)
    (char-code (aref s 0))))

;; (string:character code)
;; Return a string of length 1 with first
;; element being provided code.
(define "string:character"
  (function (c)
    (format nil "~a" (code-char c))))

;; (record? object)
;; Check if object is a record.
(define "record?"
  (function (o)
    (bool->cbool (record? o))))

;; (record type . slots)
;; Create a record with particular type, filling all of the slots.
(define "record"
  (function args
    (cl:apply #'record args)))

;; (record:type record)
;; Get type of a record
(define "record:type"
  (function (r)
    (record-type r)))

;; (record:length record)
;; Get length of a record.
(define "record:length"
  (function (r)
    (record-length r)))

;; (record:get record index)
;; Get nth field of a record.
;; 0th field stores the type.
(define "record:get"
  (function (r n)
    (record-get r n)))

;; (record:set! record index value)
;; Set nth field of a record to value.
(define "record:set!"
  (function (r n v)
    (record-set! r n v)))

;; (environment? object)
;; Create a new environment with specified parent environment.
(define "environment?"
  (function (object)
    (environment? object)))

;; (environment ?parent)
;; Create a new environment with specified parent environment.
(define "environment"
  (function args
    (environment (if (consp args)
                     (car args)
                     (intern "nil")))))

;; (environment:set! environment symbol value)
;; Set value of a symbol in environment.
(define "environment:set!"
  (function (env symbol value)
    (environment-set! env symbol value)))

;; (write-byte byte)
;; Write a byte to standard output.
(define "write-byte"
  (function (byte)
    (format t "~a" (code-char byte))
    (intern "nil")))

;; (write string) => nil
;; Write string to standard output.
(define "write"
  (function (string)
    (loop for c across string do (princ c))
    (intern "nil")))

;; (:print object)
;; A primitive printer. Extended with generics in-langauge.
(define ":print"
  (function (object)
    (print object)))

;; (== . objects)
;; Check of each of the objects are same object.
(define "=="
  (function objects
    (bool->cbool (let ((first (car objects)))
                   (dolist (o objects t)
                     (or (eq first o) (return)))))))

;; (exit code)
;; Kill the interpreter and make it return the provided exit code.
;; TODO naming?
(define "system:exit"
  (function (code)
    (uiop:die code)))

;; TODO remove
;; (die message)
;; Print message and kill current process.
(define "die"
  (function (message)
    (error "~a" message)))

;;; Core ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (let 'bindings . 'body)
;; ;; bindings => ((ptree value) ...)
;; (define "let"
;;   (special (bindings . body) env
;;     (loop with new-env = (e:new env)
;;           for (ptree value) in bindings
;;           do (i:destructure ptree
;;                             (i:eval value new-env)
;;                             (lambda (k v) (e:set! new-env k v)))
;;           finally (return (i:eval-many body new-env)))))
;;
;; ;; (quote 'object)
;; ;; Return object without evaluating it.
;; (define "quote"
;;   (special (thing) env
;;     thing))
;;
;; ;; (identity x) => x
;; ;; Return argument as is.
;; (define "identity"
;;   (function (x) x))
;;
;; ;; (evaluate-many forms env)
;; ;; Evaluate list of forms in provided environment,
;; ;; returning last result.
;; (define "evaluate-many"
;;   (function (forms env)
;;     (i:eval-many forms env)))
;;
;; ;; (apply fn args)
;; ;; (apply fn args env)
;; ;; (apply pair '(a 2)) <=> (pair 'a '2)
;; (define "apply"
;;   (function (fn args . rest)
;;     (i:apply (c:unwrap fn) args (if (consp rest)
;;                                     (car rest)
;;                                     (intern "nil")))))
;;
;; ;; (do . 'forms)
;; ;; Evaluate forms in current environment, returning last result.
;; (define "do"
;;   (special forms env
;;     (i:eval-many forms env)))

;;; Control flow ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (ifs ('test . 'body) . 'more-clauses)
;; ;; Multi-branch if.
;; (define "ifs"
;;   (special clauses env
;;     (loop for (test . body) in clauses do
;;       (when (cbool->bool (i:eval test env))
;;         (return (i:eval-many body env))))))
;;
;; ;; (when test . body)
;; ;; (when true 1 2) <=> (if true (do 1 2) nil)
;; (define "when"
;;   (special (test . body) env
;;     (if (cbool->bool (i:eval test env))
;;         (i:eval-many body env)
;;         (intern "nil"))))
;;
;; ;; (when-not test . body)
;; ;; (when-not true 1 2) <=> (if true nil (do 1 2))
;; (define "when-not"
;;   (special (test . body) env
;;     (if (cbool->bool (i:eval test env))
;;         (intern "nil")
;;         (i:eval-many body env))))
;;
;; ;; (while 'test . 'body)
;; ;; Evaluate body until test returns false.
;; (define "while"
;;   (special (test . body) env
;;     (loop while (cbool->bool (i:eval test env))
;;           for result = (i:eval-many body env)
;;           finally (return result))))
;;
;; ;; (dotimes ('symbol 'number) . 'body)
;; ;; Execute body number of times.
;; ;; TODO maybe rename?
;; (define "dotimes"
;;   (special ((symbol value) . body) env
;;     (let ((inner (e:new env))
;;           (result (intern "nil")))
;;       (dotimes (n (i:eval value env) result)
;;         (e:set! inner symbol n)
;;         (setq result (i:eval-many body inner))))))
;;
;; ;; (and . 'args)
;; (define "and"
;;   (special args env
;;     (loop named here
;;           for arg in args
;;           for x = (i:eval arg env)
;;           do (unless (cbool->bool x)
;;                (return-from here (intern "false")))
;;           finally (return-from here x))))
;;
;; ;; (or . 'args)
;; (define "or"
;;   (special args env
;;     (loop named here
;;           for arg in args
;;           for x = (i:eval arg env)
;;           do (when (cbool->bool x)
;;                (return-from here x))
;;           finally (return-from here (intern "false")))))

;;; Pairs and lists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (pair:head pair)
;; ;; (pair:head '(a . b)) => a
;; ;; Get value from head slot of a pair.
;; (define "pair:head"
;;   (function (pair)
;;     (car pair)))
;;
;; ;; (pair:tail pair)
;; ;; (pair:tail '(a . b)) => b
;; ;; Get value from head slot of a pair.
;; (define "pair:tail"
;;   (function (pair)
;;     (cdr pair)))
;;
;; ;; (list . args)
;; ;; (list 'a (+ 1 2)) => (a 3)
;; ;; Create a new list by evaluating all arguments.
;; (define "list"
;;   (function args args))
;;
;; ;; (list:first list)
;; ;; (list:first '(a b c)) => a
;; ;; Get first element of a list.
;; (define "list:first"
;;   (function (list)
;;     (car list)))
;;
;; ;; (list:second list)
;; ;; (list:second '(a b c)) => b
;; ;; Get second element of a list.
;; (define "list:second"
;;   (function (list)
;;     (cadr list)))
;;
;; ;; (list:third list)
;; ;; (list:third '(a b c)) => b
;; ;; Get third element of a list.
;; (define "list:third"
;;   (function (list)
;;     (caddr list)))
;;
;; ;; (list:rest pair)
;; ;; (list:rest '(a b c)) => (b c)
;; ;; Get value from tail slot of a pair.
;; (define "list:rest"
;;   (function (pair)
;;     (cdr pair)))
;;
;; ;; (list:length list)
;; ;; (list:length '(1 2 3)) => 3
;; ;; Return a length of a list.
;; (define "list:length"
;;   (function (list)
;;     (length list)))
;;
;; ;; (list:prepend . args)
;; ;; (list:prepend 1) <=> 1
;; ;; (list:prepend 1 2) <=> (pair 1 2)
;; ;; (list:prepend 1 2 '(3)) <=> (pair 1 (pair 2 '(3)))
;; ;; Create a new list with elements before last prepended
;; ;; to the last one.
;; (define "list:prepend"
;;   (function ls
;;     (apply #'list* ls)))
;;
;; ;; (list:fold function accumulator list)
;; ;; (list:fold + 0 '(1 2 3 4 5)) <=> (+ (+ (+ (+ (+ 1 0) 2) 3) 4) 5)
;; (define "list:fold"
;;   (c:wrap (special (f a l) env
;;             (loop for e in l
;;                   do (setq a (i:apply (c:unwrap f)
;;                                       (list a e)
;;                                       env))
;;                   finally (return a)))))
;;
;; ;; (list:fold-right function accumulator list)
;; ;; (list:fold-right + 0 '(1 2 3 4 5)) <=> (+ 1 (+ 2 (+ 3 (+ 4 (+ 5 0)))))
;; (define "list:fold-right"
;;   (c:wrap (special (f a l) env
;;             (labels ((helper (f a l)
;;                        (if (consp l)
;;                            (i:apply f
;;                                     (list (helper f a (cdr l))
;;                                           (car l))
;;                                     env)
;;                            a)))
;;               (helper (c:unwrap f) a l)))))
;;
;; ;; (list:map function list)
;; ;; (list:map pair:head '((1 . a) (2 . b))) => (1 2)
;; (define "list:map"
;;   (c:wrap (special (f l) env
;;             (mapcar (lambda (e)
;;                       (i:apply (c:unwrap f)
;;                                (list e)
;;                                env))
;;                     l))))
;;
;; ;; (list:map! function list)
;; ;; (list:map! pair:head '((1 . a) (2 . b))) => (1 2)
;; ;; Mutate the list by applying f to each of its elements.
;; (define "list:map!"
;;   (c:wrap (special (f l) env
;;             (map-into l
;;                       (lambda (e)
;;                         (i:apply (c:unwrap f)
;;                                  (list e)
;;                                  env))
;;                       l))))
;;
;; ;; (list:each function list)
;; ;; Call function for each element in a list and return nil.
;; (define "list:each"
;;   (c:wrap (special (f l) env
;;             (mapc (lambda (e)
;;                     (i:apply (c:unwrap f) (list e) env))
;;                   l)
;;             (intern "nil"))))
;;
;; ;; (list:filter function list)
;; ;; (list:filter odd? '(1 2 3 4 5)) => (1 3 5)
;; (define "list:filter"
;;   (c:wrap (special (f l) env
;;             (remove-if-not (lambda (e)
;;                              (cbool->bool (i:apply (c:unwrap f)
;;                                                    (list e)
;;                                                    env)))
;;                            l))))

;;; Functions and specials ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (function 'ptree . 'body)
;; ;; Create a new function.
;; (define "function"
;;   (special (ptree . body) env
;;     (c:wrap (c:make-special :ebind (intern "nil")
;;                             :ptree ptree
;;                             :body body
;;                             :environment env))))

;;; Numbers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (+ . numbers)
;; ;; (+ 1 2 3) => 6
;; ;; Add all numbers together.
;; (define "+"
;;   (function args
;;     (apply #'+ args)))
;;
;; ;; (* . numbers)
;; ;; (* 1 2 3) => 6
;; ;; Multiply all of the numbers together.
;; (define "*"
;;   (function args
;;     (apply #'* args)))
;;
;; ;; (exponent base power)
;; ;; (exponent 2 10) => 1024
;; ;; Return the exponential of base and power.
;; (define "exponent"
;;   (function (base power)
;;     (expt base power)))
;;
;; ;; (modulus number)
;; ;; (modulus 5) => 5
;; ;; (modulus -5) => 5
;; ;; Return absolute value of number.
;; (define "modulus"
;;   (function (number)
;;     (abs number)))
;;
;; ;; (> . numbers)
;; ;; Check that each of the numbers is strictly greater than next one.
;; (define ">"
;;   (function args
;;     (bool->cbool (apply #'> args))))
;;
;; ;; (<= . numbers)
;; ;; Check that each of the numbers is smaller or equal to next one.
;; (define "<="
;;   (function args
;;     (bool->cbool (apply #'<= args))))
;;
;; ;; (>= . numbers)
;; ;; Check that each of the numbers is greater or equal to next one.
;; (define ">="
;;   (function args
;;     (bool->cbool (apply #'>= args))))
;;
;; ;; (even? number)
;; ;; Check if number is even (divisable by 2).
;; (define "even?"
;;   (function (number)
;;     (bool->cbool (evenp number))))
;;
;; ;; (odd? number)
;; ;; Check if number is odd (not divisable by 2).
;; (define "odd?"
;;   (function (number)
;;     (bool->cbool (oddp number))))

;;; Arrays ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (array . args)
;; ;; (array 1 2 (+ 1 2)) => [1 2 3]
;; ;; Return a new array filled with args.
;; (define "array"
;;   (function args
;;     (coerce args 'vector)))
;;
;; ;; (array:copy array)
;; ;; Return a copy of an array.
;; (define "array:copy"
;;   (function (a)
;;     (copy-seq a)))
;;
;; ;; (array:fold function accumulator array)
;; ;; (array:fold + 0 [1 2 3 4 5]) <=> (+ (+ (+ (+ (+ 1 0) 2) 3) 4) 5)
;; (define "array:fold"
;;   (c:wrap (special (f a array) env
;;             (loop for e across array
;;                   do (setq a (i:apply (unwrap f)
;;                                       (list a e)
;;                                       env))
;;                   finally (return a)))))
;;
;; ;; (array:each function a) => nil
;; ;; Call function for each element of an array.
;; (define "array:each"
;;   (c:wrap (special (f a) env
;;             (loop for e across a
;;                   do (i:apply (c:unwrap f) (list e) env))
;;             (intern "nil"))))
;;
;; ;; (array:map! function a) => a
;; (define "array:map!"
;;   (c:wrap (special (f a) env
;;             (loop for i from 0 below (length a)
;;                   do (setf (aref a i)
;;                            (i:apply (c:unwrap f)
;;                                     (list (aref a i))
;;                                     env)))
;;             a)))
;;
;; ;; (array:map function a)
;; (define "array:map"
;;   (c:wrap (special (f a) env
;;             (i:eval (list (intern "array:map!")
;;                           f
;;                           (copy-seq a))
;;                     env))))

;;; Strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (string:append . strings)
;; ;; Return a new string that is a concatenation of all arguments.
;; (define "string:append"
;;   (function strings
;;     (format nil "~{~a~}" strings)))

;;; Environments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (environment:here)
;; ;; Return current environment.
;; (define "environment:here"
;;   (c:wrap (special () env env)))
;;
;; ;; (define ptree value)
;; ;; Destructure value over the ptree and set each of
;; ;; the symbols to corresponding values in current environment.
;; (define "define"
;;   (special (ptree form) env
;;     (let ((value (i:eval form env)))
;;       (i:destructure ptree value (lambda (k v)
;;                                    (e:set! env k v)))
;;       value)))

;;; Hashmaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (hashmap . args)
;; ;; Create a hashmap out of args, with each odd
;; ;; argument being the key, and each even - key's value.
;; (define "hashmap"
;;   (function args
;;     (loop with hashmap = (h:new)
;;           while args
;;           for key = (pop args)
;;           for value = (pop args)
;;           do (h:set! hashmap key value)
;;           finally (return hashmap))))
;;
;; ;; (hashmap:get h key ?default)
;; (define "hashmap:get"
;;   (function (h key . rest)
;;     (h:get h key (if (null rest)
;;                      (intern "nil")
;;                      (car rest)))))
;;
;; ;; (hashmap? object)
;; (define "hashmap?"
;;   (function (object)
;;     (h:hashmap? object)))
;; 
;; ;; (hashmap:copy h)
;; (define "hashmap:copy"
;;   (function (h) (h:copy h)))
;;
;; ;; (hashmap:in? hashmap key)
;; ;; Check ir key is present in hashmap.
;; (define "hashmap:in?"
;;   (function (hashmap key)
;;     (bool->cbool (h:find hashmap key))))
;;
;; ;; (hashmap:set! h key value)
;; (define "hashmap:set!"
;;   (function (h key value)
;;     (h:set! h key value)))
;;
;; ;; (hashmap:each function h)
;; (define "hashmap:each"
;;   (c:wrap (special (f h) env
;;             (h:each (lambda (k v)
;;                       (i:apply (c:unwrap f) (list (cons k v)) env))
;;                     h))))
;;
;; (hashmap:map function h)
;; (define "hashmap:map"
;;   (function (f h)
;;     (h:map (lambda (k v)
;;               (i:apply (unwrap f) (list (cons k v)) env))
;;             h)))
;; 
;; ;; (hashmap:new options)
;; ;; Options:
;; ;;   hash-function - function for hashing
;; ;;   capacity - how big should storage array be for the hashmap
;; (define "h:new"
;;   (function (options)
;;     (declare (ignore options))
;;     (h:new)))

;;; Other ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; (not x)
;; ;; (not false) => true
;; ;; (not 'anything-else) => false
;; ;; Invert x logically.
;; ;; If x is false, return true, otherwise return false.
;; (define "not"
;;   (function (x)
;;     (bool->cbool (if (cbool->bool x) t nil))))
;;
;; ;; (defun name . body)
;; ;; Same as (define name (function . body)).
;; (define "defun"
;;   (special (name . body) env
;;     (let ((value
;;             (i:eval (cons (intern "function")
;;                           body)
;;                     env)))
;;       (e:set! env name value)
;;       value)))
