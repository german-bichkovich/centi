(in-package :centi)

;;; Utils ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun bool->cbool (x)
  "Convert CL boolean to Centi boolean."
  (if x (intern "true") (intern "false")))

(defun cbool->bool (x)
  "Convert Centi boolean to CL boolean."
  (not (member x (list (intern "false")
                       (intern "nil")))))

(defmacro define (name thing)
  `(progn (environment-set! *stdenv* (intern ,name) ,thing)
          nil))

(defun array? (object)
  (and (arrayp object)
       (eq t (array-element-type object))))

(defun positive-integer? (object)
  (and (integerp object)
       (<= 0 object)))

(defun byte? (object)
  (and (integerp object)
       (<= 0 object 255)))

(defparameter *stdenv* (environment)
  "Standard environment that includes all builtins.")

(defun init ()
  (load "@/lisp/bootstrap" *stdenv* #'identity))

;;; Bootstrap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (if 'test 'then 'else)
;; If test is true, evaluate then form, otherwise evaluate else form.
(define "if"
  (special args env
    (assert (= (length args) 3) "if: invalid arity")
    (destructuring-bind (test then else) args
      (evaluate test
                env
                (lambda (test)
                  (if (cbool->bool test)
                      (evaluate then env k)
                      (evaluate else env k)))))))

;; (special 'ptree 'ebind . 'body)
;; Create a new special form.
(define "special"
  (special args env
    (assert (>= (length args) 2) "special: invalid arity")
    (destructuring-bind (ptree ebind . body) args
      (assert (symbol? ebind) "special: ebind must be a symbol")
      (funcall k
               (special-new :ebind ebind
                            :ptree ptree
                            :body
                            (special-remove-declaration body)
                            :environment env
                            :metadata
                            (special-extract-metadata body))))))

;; (evaluate form)
;; (evaluate form environment)
;; Evaluate form in provided or current environment.
(define "evaluate"
  (wrap (special args env
          (case (length args)
            (1
             (evaluate (car args) env k))
            (2
             (destructuring-bind (form env) args
               (assert (environment? env)
                       "evaluate: not an environment")
               (evaluate form env k)))
            (t
             (error "evaluate: invalid arity"))))))

;; (symbol name)
;; Create a symbol that is not interned.
(define "symbol"
  (function args
    (assert (= (length args) 1)  "symbol: invalid arity")
    (let ((name (car args)))
      (assert (stringp name) "symbol: name is not a string")
      (funcall k (symbol name)))))

;; (symbol? object)
;; Check if object is a symbol.
(define "symbol?"
  (function args
    (assert (= (length args) 1) "symbol?: invalid arity")
    (funcall k (bool->cbool (symbol? (car args))))))

;; (symbol:name symbol)
;; Return a string representing a symbol name.
(define "symbol:name"
  (function args
    (assert (= (length args) 1) "symbol:name: invalid arity")
    (let ((symbol (car args)))
      (assert (symbol? symbol) "symbol:name: not a symbol")
      (funcall k (symbol-name symbol)))))

;; (symbol:intern name)
;; Intern a symbol with provided name.
;; Two different outputs of (intern "x") are always the same objects.
(define "symbol:intern"
  (function args
    (assert (= (length args) 1) "symbol:intern: invalid arity")
    (let ((name (car args)))
      (assert (stringp name) "symbol:intern: name is not a string")
      (funcall k (intern name)))))

;; (pair? object)
;; Check if object is a pair.
(define "pair?"
  (function args
    (assert (= (length args) 1) "pair?: invalid arity")
    (funcall k (bool->cbool (consp (car args))))))

;; (pair head tail)
;; Construct a pair.
(define "pair"
  (function args
    (assert (= (length args) 2) "pair: invalid arity")
    (destructuring-bind (head tail) args
      (funcall k (cons head tail)))))

;; (pair:head! pair value)
;; Store value in head slot of a pair.
(define "pair:head!"
  (function args
    (assert (= (length args) 2) "pair:head!: invalid arity")
    (destructuring-bind (pair value) args
      (assert (consp pair) "pair:head!: not a pair")
      (rplaca pair value)
      (funcall k pair))))

;; (pair:tail! pair value)
;; Store value in tail slot of a pair.
(define "pair:tail!"
  (function args
    (assert (= (length args) 2) "pair:tail!: invalid arity")
    (destructuring-bind (pair value) args
      (assert (consp pair) "pair:tail!: not a pair")
      (rplacd pair value)
      (funcall k pair))))

;; (function? object)
;; Check if object is a function.
(define "function?"
  (function args
    (assert (= (length args) 1) "function?: invalid arity")
    (funcall k (bool->cbool (function? (car args))))))

;; (special? object)
;; Check if object is a special form.
(define "special?"
  (function args
    (assert (= (length args) 1) "special?: invalid arity")
    (funcall k (bool->cbool (special? (car args))))))

;; (unwrap function)
;; Return a special form with same body as one function has.
(define "unwrap"
  (function args
    (assert (= (length args) 1) "unwrap: invalid arity")
    (let ((function (car args)))
      (assert (function? function) "unwrap: not a function")
      (funcall k (unwrap function)))))

;; (wrap special)
;; Return a function with same body as provided special has.
(define "wrap"
  (function args
    (assert (= (length args) 1) "wrap: invalid arity")
    (let ((special (car args)))
      (assert (special? special) "wrap: not a special")
      (funcall k (wrap special)))))

;; (number? object)
;; Check if object is a number.
(define "number?"
  (function args
    (assert (= (length args) 1) "number?: invalid arity")
    (funcall k (bool->cbool (integerp (car args))))))

;; (- number)
;; (- 1) => -1
;; (- number . numbers)
;; (- 9 6 3) => 0
;; In first form, return number negated.
;; In second form, subtract each of numbers from number.
(define "-"
  (function args
    (assert (>= (length args) 1) "-: invalid arity")
    (assert (every #'integerp args) "-: not a number")
    (funcall k (cl:apply #'- args))))

;; (/ divident . divisors)
;; Divide divident by each of the divisors in a sequence.
(define "/"
  (function args
    (assert (>= (length args) 1) "/: invalid arity")
    (assert (every #'integerp args) "/: not a number")
    (assert (every (lambda (x) (not (zerop x)))
                   (cdr args))
            "/: not a number")
    (funcall k (truncate (cl:apply #'/ args)))))

;; (modulo divident divisor)
;; Divide divident by divisor and return the remainder.
(define "modulo"
  (function args
    (assert (= (length args) 2) "modulo: invalid arity")
    (destructuring-bind (divident divisor) args
      (assert (integerp divident) "modulo: divident is not a number")
      (assert (integerp divisor) "modulo: divisor is not a number")
      (funcall k (mod divident divisor)))))

;; (< number . numbers)
(define "<"
  (function args
    (assert (>= (length args) 1) "<: invalid arity")
    (assert (every #'integerp args) "<: not a number")
    (funcall k (bool->cbool (cl:apply #'< args)))))

;; (<< number n)
;; Bit shift number by n bits to the left.
(define "<<"
  (function args
    (assert (= (length args) 2) "<<: invalid arity")
    (destructuring-bind (number n) args
      (assert (integerp number) "<<: not a number")
      (assert (integerp n) "<<: n is not a number")
      (assert (> n 0) "<<: n is not a positive number")
      (funcall k (ash number n)))))

;; (>> number n)
;; Bit shift number by n bits to the right.
(define ">>"
  (function args
    (assert (= (length args) 2) ">>: invalid arity")
    (destructuring-bind (number n) args
      (assert (integerp number) ">>: not a number")
      (assert (integerp n) ">>: n is not a number")
      (assert (> n 0) ">>: n is not a positive number")
      (funcall k (ash number (- n))))))

;; (bitwise-and . numbers)
;; Return a bitwise AND of all provided numbers.
(define "bitwise-and"
  (function args
    (assert (every #'integerp args) "bitwise-and: not a number")
    (funcall k (cl:apply #'logand args))))

;; (bitwise-or . numbers)
;; Return a bitwise OR of all provided numbers.
(define "bitwise-or"
  (function args
    (assert (every #'integerp args) "bitwise-or: not a number")
    (funcall k (cl:apply #'logior args))))

;; (bitwise-exclusive-or number)
;; Return a bitwise XOR of all provided numbers.
(define "bitwise-exclusive-or"
  (function args
    (assert (every #'integerp args)
            "bitwise-exclusive-or: not a number")
    (funcall k (cl:apply #'logxor args))))

;; (bitwise-not number)
;; Return a bitwise NOT of a number.
(define "bitwise-not"
  (function args
    (assert (= (length args) 1) "bitwise-not: invalid arity")
    (let ((number (car args)))
      (assert (integerp number) "bitwise-not: not a number")
      (funcall k (cl:apply #'lognot args)))))

;; (array? object)
;; Check if object is an array.
(define "array?"
  (function args
    (assert (= (length args) 1) "array?: invalid arity")
    (funcall k (bool->cbool (array? (car args))))))

;; (array:length array)
;; Get length of an array.
(define "array:length"
  (function args
    (assert (= (length args) 1) "array:length: invalid arity")
    (let ((array (car args)))
      (assert (array? array) "array:length: not an array")
        (funcall k (length array)))))

;; (array::new length default)
;; Primitive constructor for a general array.
(define "array::new"
  (function args
    (assert (= (length args) 2) "array::new: invalid arity")
    (destructuring-bind (length default) args
      (assert (integerp length) "array::new: length is not a number")
      (assert (>= length 0) "array::new: length is not positive")
      (funcall k
               (make-array length
                           :initial-element default
                           :element-type t)))))

;; (array:get array index)
;; (array:get array index default)
;; Get element of an array at index.
(define "array:get"
  (function args
    (assert (<= 2 (length args) 3) "array:get: invalid arity")
    (destructuring-bind
        (array index &optional (default (intern "nil"))) args
      (assert (array? array) "array:get: not an array")
      (assert (integerp index) "array:get: index is not an integer")
      (assert (>= index 0) "array:get: index is invalid")
      (funcall k (if (< -1 index (length array))
                     (aref array index)
                     default)))))

;; (array:set! array index value)
;; Set element of an array at index.
(define "array:set!"
  (function args
    (assert (= (length args) 3) "array:set!: invalid arity")
    (destructuring-bind (array index value) args
      (assert (array? array) "array:set!: not an array")
      (assert (integerp index) "array:set!: index is not an integer")
      (assert (>= index 0) "array:set!: index is not positive")
      (setf (aref array index) value)
      (funcall k array))))

;; (string? object)
;; Check if object is a string.
(define "string?"
  (function args
    (assert (= (length args) 1) "string?: invalid arity")
    (funcall k (bool->cbool (stringp (car args))))))

;; (string:length string)
;; Get length of a string.
(define "string:length"
  (function args
    (assert (= (length args) 1) "string:length: invalid arity")
    (let ((string (car args)))
      (assert (stringp string) "string:length: not a string")
      (funcall k (length string)))))

;; (string::new length default)
;; A primitive constructor for a string.
(define "string::new"
  (function args
    (assert (= (length args) 2) "string::new: invalid arity")
    (destructuring-bind (length default) args
      (assert (positive-integer? length)
              "string::new: invalid length")
      (assert (positive-integer? default)
              "string::new: default is not a character")
      (let ((default (code-char default)))
        (funcall k
                 (make-array length
                             :initial-element default
                             :element-type 'character))))))

;; (string:get array index)
;; (string:get array index default)
;; Get element of an array at index.
;; TODO get nth character vs get nth byte?
(define "string:get"
  (function args
    (assert (<= 2 (length args) 3) "string:get: invalid arity")
    (destructuring-bind
        (string index &optional (default 0)) args
      (assert (stringp string) "string:get: not a string")
      (assert (positive-integer? index)
              "string:get: index is invalid")
      (assert (positive-integer? default)
              "string:get: default is not a character")
      (funcall k (if (< -1 index (length string))
                     (char-code (aref string index))
                     default)))))

;; (string:set! string index value)
;; Set element of an array at index.
(define "string:set!"
  (function args
    (assert (= (length args) 3) "string:set!: invalid arity")
    (destructuring-bind (string index value) args
      (assert (stringp string) "string:set!: not a string")
      (assert (positive-integer? index) "string:set!: invalid index")
      (setf (aref string index) (code-char value))
      (funcall k string))))

;; (string:code string)
;; Return the code encoded in first character of the string.
(define "string:code"
  (function args
    (assert (= (length args) 1) "string:code: invalid arity")
    (let ((string (car args)))
      (assert (stringp string) "string:code: not a string")
      (assert (= (length string) 1) "string:code: invalid format")
      (funcall k (char-code (aref (car args) 0))))))

;; (string:character code)
;; Return a string of length 1 with first element being provided code.
(define "string:character"
  (function args
    (assert (= (length args) 1) "string:character: invalid arity")
    (let ((code (car args)))
      (assert (positive-integer? code)
              "string:character: invalid code")
      (funcall k (format nil "~a" (code-char code))))))

;; (record? object)
;; Check if object is a record.
(define "record?"
  (function args
    (assert (= (length args) 1) "record?: invalid arity")
    (funcall k (bool->cbool (record? (car args))))))

;; (record type . slots)
;; Create a record with particular type, filling all of the slots.
;; TODO hide?
(define "record"
  (function args
    (assert (>= (length args) 1) "record: invalid arity")
    (assert (array? (car args)) "record: invalid type")
    (funcall k (cl:apply #'record args))))

;; (record:type record)
;; Get type of a record
(define "record:type"
  (function args
    (assert (= (length args) 1) "record:type: invalid arity")
    (let ((record (car args)))
      (assert (record? record) "record:type: not a record")
      (funcall k (record-type (car args))))))

;; (record:length record)
;; Get length of a record.
(define "record:length"
  (function args
    (assert (= (length args) 1) "record:length: invalid arity")
    (let ((record (car args)))
      (assert (record? record) "record:length: not a record")
      (funcall k (record-length record)))))

;; (record:nth record n)
;; Get nth field of a record.
(define "record:nth"
  (function args
    (assert (= (length args) 2) "record:nth: invalid arity")
    (destructuring-bind (record n) args
      (assert (< -1 n (record-length record))
              "record:nth: out of bounds")
      (funcall k (record-nth record n)))))

;; (record:set! record index value)
;; Set nth field of a record to value.
(define "record:set!"
  (function args
    (assert (= (length args) 3) "record:set!: invalid arity")
    (destructuring-bind (record index value) args
      (assert (record? record) "record:set!: not a record")
      (assert (positive-integer? index) "record:set!: invalid index")
      (funcall k (record-set! record index value)))))

;; (environment? object)
;; Create a new environment with specified parent environment.
(define "environment?"
  (function args
    (assert (= (length args) 1) "environment?: invalid arity")
    (funcall k (bool->cbool (environment? (car args))))))

;; (environment)
;; (environment parent)
;; Create a new environment with specified parent environment.
(define "environment"
  (function args
    (case (length args)
      (0
       (funcall k (environment (intern "nil"))))
      (1
       (let ((parent (car args)))
         (assert (environment? parent)
                 "environment: parent is not an environment")
         (funcall k (environment parent))))
      (t
       (error "environment: invalid arity")))))

;; (environment:set! environment symbol value)
;; Set value of a symbol 'symbol' in environment.
(define "environment:set!"
  (function args
    (assert (= (length args) 3) "environment:set!: invalid arity")
    (destructuring-bind (environment symbol value) args
      (assert (environment? environment)
              "environment:set!: not an environment")
      (assert (symbol? symbol)
              "environment:set!: not a symbol")
      (funcall k
               (environment-set! environment symbol value)))))

;; (write-byte byte)
;; Write a byte to standard output.
(define "write-byte"
  (function args
    (assert (= (length args) 1) "write-byte: invalid arity")
    (let ((byte (car args)))
      (assert (byte? byte) "write-byte: not a byte")
      (format t "~a" (code-char (car args)))
      (funcall k (intern "nil")))))

;; (write string) => nil
;; Write string to standard output.
(define "write"
  (function args
    (assert (= (length args) 1) "write: invalid arity")
    (let ((string (car args)))
      (assert (stringp string) "write: not a string")
      (loop for c across (car args) do (princ c))
      (funcall k (intern "nil")))))

;; (:print object)
;; A primitive printer. Extended with generics in-langauge.
(define ":print"
  (function args
    (assert (= (length args) 1) ":print: invalid arity")
    (funcall k (print (car args)))))

;; (== . objects)
;; Check of each of the objects are same object.
(define "=="
  (function args
    (funcall k
             (bool->cbool (let ((first (car args)))
                            (dolist (o args t)
                              (or (eq first o) (return))))))))

;; (exit code)
;; Kill the interpreter and make it return the provided exit code.
(define "exit"
  (function args
    (assert (= (length args) 1) "exit: invalid arity")
    (let ((code (car args)))
      (assert (byte? code) "exit: invalid code")
      (funcall k (uiop:quit code)))))

;; (load file-spec)
(define "load"
  (function args env
    (assert (= (length args) 1) "load: invalid arity")
    (let ((path (car args)))
      (assert (stringp path) "invalid file specification")
      (load path (environment env) k))))

;;; Questionable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO remove
;; TODO drop continuation properly
;; (die message)
;; Print message and kill current process.
(define "die"
  (function args
    (error "~a" (car args))))

;; (environment:find environment symbol)
;; Find an environment in which symbol is bound.
(define "environment:find"
  (function args
    (assert (= (length args) 2) "environment:find: invalid arity")
    (destructuring-bind (environment symbol) args
      (assert (environment? environment)
              "environment:find: not an environment")
      (assert (symbol? symbol) "environment:find: not a symbol")
      (let ((e (environment-find environment symbol)))
        (funcall k (or e (intern "false")))))))

;; (environment::parent environment)
(define "environment::parent"
  (function args
    (if (= (length args) 1)
        (let ((environment (car args)))
          (if (environment? environment)
              (funcall k (environment-parent environment))
              (error "not an environment")))
        (error "environment::parent: invalid arity"))))

(define "special::ebind"
  (function args
    (funcall k (special-ebind (car args)))))

(define "special::body"
  (function args
    (funcall k (special-body (car args)))))

(define "special::ptree"
  (function args
    (funcall k (special-ptree (car args)))))

(define "special::environment"
  (function args
    (funcall k (special-environment (car args)))))

(define "special::metadata"
  (function args
    (funcall k (special-metadata (car args)))))
