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

;;; Bootstrap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (evaluate form)
;; (evaluate form environment)
;; Evaluate form in provided or current environment.
(define "evaluate"
  (wrap (special args env
          (destructuring-bind (form &optional (env env)) args
            (eval form env)))))

(setf (special-metadata (unwrap (eval (intern "evaluate") *stdenv*)))
      (read-string "((name . evaluate))"))

;; (if 'test 'then 'else)
;; If test is true, evaluate then form, otherwise evaluate else form.
(define "if"
  (special args env
    (destructuring-bind (test then else) args
      (if (cbool->bool (eval test env))
          (eval then env)
          (eval else env)))))

;; (symbol name)
;; Create a symbol that is not interned.
(define "symbol"
  (function args
    (symbol (car args))))

;; (symbol? object)
;; Check if object is a symbol.
(define "symbol?"
  (function args
    (let ((object (car args)))
      (bool->cbool (or (symbol? object)
                       (eq object nil))))))

;; (symbol:name symbol)
;; Return a string representing a symbol name.
(define "symbol:name"
  (function args
    (symbol-name (car args))))

;; (symbol:intern name)
;; Intern a symbol with provided name.
;; Two different outputs of (intern "x") are always the same objects.
(define "symbol:intern"
  (function args
    (intern (car args))))

;; (pair? object)
;; Check if object is a pair.
(define "pair?"
  (function args
    (bool->cbool (consp (car args)))))

;; (pair head tail)
;; Construct a pair.
(define "pair"
  (function args
    (cons (car args) (cadr args))))

;; (pair:head! pair value)
;; Store value in head slot of a pair.
(define "pair:head!"
  (function args
    (destructuring-bind (pair object) args
      (rplaca pair object)
      pair)))

;; (pair:tail! pair value)
;; Store value in tail slot of a pair.
(define "pair:tail!"
  (function args
    (destructuring-bind (pair object) args
      (rplacd pair object)
      pair)))

;; (function? object)
;; Check if object is a function.
(define "function?"
  (function args
    (bool->cbool (function? (car args)))))

;; (special? object)
;; Check if object is a special form.
(define "special?"
  (function args
    (bool->cbool (special? (car args)))))

;; (special 'ptree 'ebind . 'body)
;; Create a new special form.
(define "special"
  (special args env
    (destructuring-bind (ptree ebind . body) args
      (special-new :ebind ebind
                   :ptree ptree
                   :body (special-remove-declaration body)
                   :environment env
                   :metadata (special-extract-metadata body)))))

(setf (special-metadata (eval (intern "special")
                              *stdenv*))
      (read-string "((name . special)
                     (pure . true))"))

;; (unwrap function)
;; Return a special form with same body as one function has.
(define "unwrap"
  (function args
    (unwrap (car args))))

;; (wrap special)
;; Return a function with same body as provided special has.
(define "wrap"
  (function args
    (wrap (car args))))

(setf (special-metadata (unwrap (eval (intern "wrap") *stdenv*)))
      (read-string "((name . wrap) (pure . true))"))

;; (number? object)
;; Check if object is a number.
(define "number?"
  (function args
    (bool->cbool (numberp (car args)))))

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
  (function args
    (ash (car args) (cadr args))))

;; (bitwise-and . numbers)
;; Return a bitwise AND of all provided numbers.
(define "bitwise-and"
  (function args
    (cl:apply #'logand args)))

;; (bitwise-or . numbers)
;; Return a bitwise OR of all provided numbers.
(define "bitwise-or"
  (function args
    (cl:apply #'logior args)))

;; (bitwise-exclusive-or number)
;; Return a bitwise XOR of all provided numbers.
(define "bitwise-exclusive-or"
  (function args
    (cl:apply #'logxor args)))

;; (bitwise-not number)
;; Return a bitwise NOT of a number.
(define "bitwise-not"
  (function args
    (lognot (car args))))

;; (>> number n)
;; Bit shift number by n bits to the right.
(define ">>"
  (function args
    (ash (car args) (- (cadr args)))))

;; (array? object)
;; Check if object is an array.
(define "array?"
  (function args
    (let ((object (car args)))
      (bool->cbool (and (arrayp object)
                        (eq t (array-element-type object)))))))

;; (array:length array)
;; Get length of an array.
(define "array:length"
  (function args
    (length (car args))))

;; (array::new length default)
;; Primitive constructor for a general array.
(define "array::new"
  (function args
    (destructuring-bind (length &optional (default nil default?)) args
      (make-array length
                  :initial-element (if default?
                                       default
                                       (intern "nil"))
                  :element-type t))))

;; (array:get array index)
;; (array:get array index default)
;; Get element of an array at index.
(define "array:get"
  (function args
    (destructuring-bind (a n . rest) args
      (if (< -1 n (length a))
          (aref a n)
          (or (car rest) (intern "nil"))))))

;; (array:set! array index value)
;; Set element of an array at index.
(define "array:set!"
  (function args
    (destructuring-bind (a n v) args
      (setf (aref a n) v)
      a)))

;; (string? object)
;; Check if object is a string.
(define "string?"
  (function args
    (let ((object (car args)))
      (bool->cbool (and (arrayp object)
                        (eq 'character
                            (array-element-type object)))))))

;; (string:length string)
;; Get length of a string.
(define "string:length"
  (function args
    (length (car args))))

;; (string::new length default)
;; A primitive constructor for a string.
(define "string::new"
  (function args
    (destructuring-bind (length &optional (default 0)) args
      (make-array length
                  :initial-element (code-char default)
                  :element-type 'character))))

;; (string:get array index)
;; Get element of an array at index.
;; TODO get nth character vs get nth byte?
(define "string:get"
  (function args
    (destructuring-bind (a n) args
      (char-code (aref a n)))))

;; (string:set! array index value)
;; Set element of an array at index.
(define "string:set!"
  (function args
    (destructuring-bind (a n v) args
      (setf (aref a n) (code-char v))
      a)))

;; (string:code string)
;; Return the code encoded in first character of the string.
(define "string:code"
  (function args
    (char-code (aref (car args) 0))))

;; (string:character code)
;; Return a string of length 1 with first
;; element being provided code.
(define "string:character"
  (function args
    (format nil "~a" (code-char (car args)))))

;; (record? object)
;; Check if object is a record.
(define "record?"
  (function args
    (bool->cbool (record? (car args)))))

;; (record type . slots)
;; Create a record with particular type, filling all of the slots.
(define "record"
  (function args
    (cl:apply #'record args)))

;; (record:type record)
;; Get type of a record
(define "record:type"
  (function args
    (record-type (car args))))

;; (record:length record)
;; Get length of a record.
(define "record:length"
  (function args
    (record-length (car args))))

;; (record:nth record n)
;; Get nth field of a record.
;; 0th field stores the type.
(define "record:nth"
  (function args
    (destructuring-bind (r n) args
      (record-nth r n))))

;; (record:set! record index value)
;; Set nth field of a record to value.
(define "record:set!"
  (function args
    (destructuring-bind (r n v) args
      (record-set! r n v))))

;; (environment? object)
;; Create a new environment with specified parent environment.
(define "environment?"
  (function args
    (bool->cbool (environment? (car args)))))

;; (environment)
;; (environment parent)
;; Create a new environment with specified parent environment.
(define "environment"
  (function args
    (environment (if (consp args)
                     (car args)
                     (intern "nil")))))

;; (environment:set! environment symbol value)
;; Set value of a symbol in environment.
(define "environment:set!"
  (function args
    (destructuring-bind (env symbol value) args
      (environment-set! env symbol value))))

;; (environment:find environment symbol)
;; Find an environment in which symbol is bound.
(define "environment:find"
  (function args
    (destructuring-bind (env symbol) args
      (let ((e (environment-find env symbol)))
        (or e (intern "false"))))))

;; (environment::parent environment)
(define "environment::parent"
  (function args
    (environment-parent (car args))))

;; (write-byte byte)
;; Write a byte to standard output.
(define "write-byte"
  (function args
    (format t "~a" (code-char (car args)))
    (intern "nil")))

;; (write string) => nil
;; Write string to standard output.
(define "write"
  (function args
    (loop for c across (car args) do (princ c))
    (intern "nil")))

;; (:print object)
;; A primitive printer. Extended with generics in-langauge.
(define ":print"
  (function args
    (print (car args))))

;; (== . objects)
;; Check of each of the objects are same object.
(define "=="
  (function args
    (bool->cbool (let ((first (car args)))
                   (dolist (o args t)
                     (or (eq first o) (return)))))))

;; (exit code)
;; Kill the interpreter and make it return the provided exit code.
(define "exit"
  (function args
    (uiop:quit (car args))))

;; TODO remove
;; (die message)
;; Print message and kill current process.
(define "die"
  (function args
    (error "~a" (car args))))

(define "special::ebind"
  (function args
    (let ((special (car args)))
      (assert (special? special))
      (special-ebind special))))

(define "special::body"
  (function args
    (let ((special (car args)))
      (assert (special? special))
      (special-body special))))

(define "special::ptree"
  (function args
    (let ((special (car args)))
      (assert (special? special))
      (special-ptree special))))

(define "special::environment"
  (function args
    (let ((special (car args)))
      (assert (special? special))
      (special-environment special))))

(define "special::metadata"
  (function args
    (let ((special (car args)))
      (assert (special? special))
      (special-metadata special))))

;; (load file-spec)
(define "load"
  (function args
    (load (car args))))
