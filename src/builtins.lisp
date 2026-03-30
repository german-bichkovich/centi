(in-package :centi)

(setq *stdenv* (environment))

(defun init ()
  (load "@/lisp/bootstrap" *stdenv* #'identity))

;;; Core ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (if 'test 'then 'else)
;; If test is true, evaluate then form, otherwise evaluate else form.
(define "if"
  (lambda (a e k)
    (if (= (length a) 3)
      (destructuring-bind (test then else) a
        (evaluate test
                  e
                  (lambda (test)
                    (if (cbool->bool test)
                        (evaluate then e k)
                        (evaluate else e k)))))
      (error "if: invalid arity"))))

;; (special 'ptree 'ebind . 'body)
;; Create a new special form.
(define "special"
  (lambda (a e k)
    (if (>= (length a) 2)
        (destructuring-bind (ptree ebind . body) a
          (if (symbol? ebind)
              (funcall k
                       (special-new :ebind ebind
                                    :ptree ptree
                                    :body
                                    (special-remove-declaration body)
                                    :environment e
                                    :metadata
                                    (special-extract-metadata body)))
              (error "special: ebind must be a symbol")))
         (error "special: invalid arity"))))

;; (evaluate form)
;; (evaluate form environment)
;; Evaluate form in provided or current environment.
(define "evaluate"
  (wrap (lambda (a e k)
          (case (length a)
            (1
             (evaluate (car a) e k))
            (2
             (destructuring-bind (form environment) a
               (if (environment? environment)
                   (evaluate form environment k)
                   (error "evaluate: not an environment"))))
            (t
             (error "evaluate: invalid arity"))))))

;; (== . objects)
;; Check of each of the objects are same object.
(define "=="
  (wrap (lambda (a e k)
          (declare (ignore e))
          (funcall k
                   (bool->cbool (let ((first (car a)))
                                  (dolist (o a t)
                                    (or (eq first o) (return)))))))))

;; (exit code)
;; Kill the interpreter and make it return the provided exit code.
(define "exit"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((code (car a)))
                (if (byte? code)
                    (funcall k (uiop:quit code))
                    (error "exit: invalid code")))
              (error "exit: invalid arity")))))

;; (load file-spec)
(define "load"
  (wrap (lambda (a e k)
          (if (= (length a) 1)
              (let ((path (car a)))
                (if (stringp path)
                    (load path (environment e) k)
                    (error "invalid file specification")))
              (error "load: invalid arity")))))

;; TODO not tail recursive yet
;; (reset (lambda () body...))
(define ":reset"
  (wrap (lambda (a e k)
          (if (= (length a) 1)
              (let ((f (car a)))
                (if (function? f)
                    (funcall k
                             (apply f () e #'identity))
                    (error "reset: not a function")))
              (error "reset: invalid arity")))))

;; (shift (lambda (k) body...))
(define "shift"
  (wrap (lambda (a e k)
          (if (= (length a) 1)
              (let ((f (car a)))
                (if (function? f)
                    (apply f
                           (list
                            (wrap (lambda (a e k2)
                                    (declare (ignore e))
                                    (if (= (length a) 1)
                                        (funcall k2 (funcall k
                                                             (car a)))
                                        (error "invalid arity")))))
                           e
                           #'identity)
                    (error "shift: not a function")))
          (error "shift: invalid arity")))))

;;; Symbols ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (symbol? object)
;; Check if object is a symbol.
(define "symbol?"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (funcall k (bool->cbool (symbol? (car a))))
              (error "symbol?: invalid arity")))))

;; (symbol name)
;; Create a symbol that is not interned.
(define "symbol"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((name (car a)))
                (if (stringp name)
                    (funcall k (symbol name))
                    (error "symbol: name is not a string")))
              (error "symbol: invalid arity")))))

;; (symbol:name symbol)
;; Return a string representing a symbol name.
(define "symbol:name"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((symbol (car a)))
                (if (symbol? symbol)
                    (funcall k (symbol-name symbol))
                    (error "symbol:name: not a symbol")))
              (error "symbol:name: invalid arity")))))

;; (symbol:intern name)
;; Intern a symbol with provided name.
;; Two different outputs of (intern "x") are always the same objects.
(define "symbol:intern"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((name (car a)))
                (if (stringp name)
                    (funcall k (intern name))
                    (error "symbol:intern: name is not a string")))
              (error "symbol:intern: invalid arity")))))

;;; Pairs & lists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (pair? object)
;; Check if object is a pair.
(define "pair?"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (funcall k (bool->cbool (consp (car a))))
              (error "pair?: invalid arity")))))

;; (pair head tail)
;; Construct a pair.
(define "pair"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 2)
              (destructuring-bind (head tail) a
                (funcall k (cons head tail)))
              (error "pair: invalid arity")))))

;; (pair:head! pair value)
;; Store value in head slot of a pair.
(define "pair:head!"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 2)
              (destructuring-bind (pair value) a
                (if (consp pair)
                    (progn (rplaca pair value)
                           (funcall k pair))
                    (error "pair:head!: not a pair")))
              (error "pair:head!: invalid arity")))))

;; (pair:tail! pair value)
;; Store value in tail slot of a pair.
(define "pair:tail!"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 2)
              (destructuring-bind (pair value) a
                (if (consp pair)
                    (progn (rplacd pair value)
                           (funcall k pair))
                    (error "pair:tail!: not a pair")))
              (error "pair:tail!: invalid arity")))))

;;; Callables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (function? object)
;; Check if object is a function.
(define "function?"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (funcall k (bool->cbool (function? (car a))))
              (error "function?: invalid arity")))))

;; (special? object)
;; Check if object is a special form.
(define "special?"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((object (car a)))
                (funcall k (bool->cbool (or (special? object)
                                            (functionp object)))))
              (error "special?: invalid arity")))))

;; (wrap special)
;; Return a function with same body as provided special has.
(define "wrap"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((special (car a)))
                (if (special? special)
                    (funcall k (wrap special))
                    (error "wrap: not a special")))
              (error "wrap: invalid arity")))))

;; (unwrap function)
;; Return a special form with same body as one function has.
(define "unwrap"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((function (car a)))
                (if (function? function)
                    (funcall k (unwrap function))
                    (error "unwrap: not a function")))
              (error "unwrap: invalid arity")))))

;;; Strings ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (string? object)
;; Check if object is a string.
(define "string?"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (funcall k (bool->cbool (stringp (car a))))
              (error "string?: invalid arity")))))

;; (string:length string)
;; Get length of a string.
(define "string:length"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((string (car a)))
                (if (stringp string)
                    (funcall k (length string))
                    (error "string:length: not a string")))
              (error "string:length: invalid arity")))))

;; (string::new length default)
;; A primitive constructor for a string.
(define "string::new"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 2)
              (destructuring-bind (length default) a
                (cond ((not (positive-integer? length))
                       (error "string::new: invalid length"))
                      ((not (positive-integer? default))
                       (error
                        "string::new: default is not a character"))
                      (t
                       (let ((default (code-char default)))
                         (funcall k
                                  (make-array length
                                              :initial-element default
                                              :element-type
                                              'character))))))
              (error "string::new: invalid arity")))))

;; (string:get string index)
;; (string:get string index default)
;; Get element of an array at index.
;; TODO get nth character vs get nth byte?
(define "string:get"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (<= 2 (length a) 3)
              (destructuring-bind
                  (string index &optional (default 0)) a
                (cond ((not (stringp string))
                       (error "string:get: not a string"))
                      ((not (positive-integer? index))
                       (error "string:get: index is invalid"))
                      ((not (positive-integer? default))
                       (error "string:get: default is not a character"))
                      (t
                       (funcall k (if (< -1 index (length string))
                                      (char-code (aref string index))
                                      default)))))
              (error "string:get: invalid arity")))))

;; (string:set! string index value)
;; Set element of an array at index.
(define "string:set!"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 3)
              (destructuring-bind (string index value) a
                (cond ((not (stringp string))
                       (error "string:set!: not a string"))
                      ((not (positive-integer? index))
                       (error "string:set!: invalid index"))
                      (t
                       (setf (aref string index) (code-char value))
                       (funcall k string))))
              (error "string:set!: invalid arity")))))

;; (string:code string)
;; Return the code encoded in first character of the string.
(define "string:code"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((string (car a)))
                (cond ((not (stringp string))
                       (error "string:code: not a string"))
                      ((not (= (length string) 1))
                       (error "string:code: invalid format"))
                      (t 
                       (funcall k (char-code (aref (car a) 0))))))
              (error "string:code: invalid arity")))))

;; (string:character code)
;; Return a string of length 1 with first element being provided code.
(define "string:character"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((code (car a)))
                (if (positive-integer? code)
                    (funcall k (format nil "~a" (code-char code)))
                    (error "string:character: invalid code")))
              (error "string:character: invalid arity")))))

;;; Arrays ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (array? object)
;; Check if object is an array.
(define "array?"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (funcall k (bool->cbool (array? (car a))))
              (error "array?: invalid arity")))))

;; (array:length array)
;; Get length of an array.
(define "array:length"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((array (car a)))
                (if (array? array)
                    (funcall k (length array))
                    (error "array:length: not an array")))
              (error "array:length: invalid arity")))))

;; (array::new length default)
;; Primitive constructor for a general array.
(define "array::new"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 2)
              (destructuring-bind (length default) a
                (cond ((not (integerp length))
                       (error "array::new: length is not a number"))
                      ((not (>= length 0))
                       (error "array::new: length is not positive"))
                      (t
                       (funcall k
                                (make-array length
                                            :initial-element default
                                            :element-type t)))))
              (error "array::new: invalid arity")))))

;; (array:get array index)
;; (array:get array index default)
;; Get element of an array at index.
(define "array:get"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (<= 2 (length a) 3)
              (destructuring-bind
                  (array index &optional (default (intern "nil"))) a
                (cond ((not (array? array))
                       (error "array:get: not an array"))
                      ((not (positive-integer? index))
                       (error "array:get: index is not an integer"))
                      ((not (>= index 0))
                       "array:get: index is invalid")
                      (t
                       (funcall k (if (< -1 index (length array))
                                      (aref array index)
                                      default)))))
              (error "array:get: invalid arity")))))

;; (array:set! array index value)
;; Set element of an array at index.
(define "array:set!"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 3)
              (destructuring-bind (array index value) a
                (cond ((not (array? array))
                       (error "array:set!: not an array"))
                      ((not (integerp index))
                       (error "array:set!: index is not an integer"))
                      ((not (>= index 0))
                       (error "array:set!: index is not positive"))
                      (t
                       (setf (aref array index) value)
                       (funcall k array))))
              (error "array:set!: invalid arity")))))

;;; Numbers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (number? object)
;; Check if object is a number.
(define "number?"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (funcall k (bool->cbool (integerp (car a))))
              (error "number?: invalid arity")))))

;; (- number)
;; (- 1) => -1
;; (- number . numbers)
;; (- 9 6 3) => 0
;; In first form, return number negated.
;; In second form, subtract each of numbers from number.
(define "-"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (>= (length a) 1)
              (if (every #'integerp a)
                  (funcall k (cl:apply #'- a))
                  (error "-: invalid arity"))
              (error "-: invalid arity")))))

;; (/ divident . divisors)
;; Divide divident by each of the divisors in a sequence.
(define "/"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (cond ((not (>= (length a) 1))
                 (error "/: invalid arity"))
                ((not (every #'integerp a))
                 (error "/: not a number"))
                ((not (every (lambda (x) (not (zerop x)))
                             (cdr a)))
                 (error "/: can't divide by 0"))
                (t
                 (funcall k (truncate (cl:apply #'/ a))))))))

;; (modulo divident divisor)
;; Divide divident by divisor and return the remainder.
(define "modulo"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 2)
              (destructuring-bind (divident divisor) a
                (cond ((not (integerp divident))
                       (error "modulo: divident is not a number"))
                      ((not (integerp divisor))
                       (error "modulo: divisor is not a number"))
                      (t
                       (funcall k (mod divident divisor)))))
              (error "modulo: invalid arity")))))

;; (< number . numbers)
(define "<"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (>= (length a) 1)
              (if (every #'integerp a)
                  (funcall k (bool->cbool (cl:apply #'< a)))
                  (error "<: not a number"))
              (error "<: invalid arity")))))

;; (<< number n)
;; Bit shift number by n bits to the left.
(define "<<"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 2)
              (destructuring-bind (number n) a
                (cond ((not (integerp number))
                       (error "<<: not a number"))
                      ((not (positive-integer? n))
                       (error "<<: n is not a positive number"))
                      (t
                       (funcall k (ash number n)))))
              (error "<<: invalid arity")))))

;; (>> number n)
;; Bit shift number by n bits to the right.
(define ">>"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 2)
              (destructuring-bind (number n) a
                (cond ((not (integerp number))
                       (error ">>: not a number"))
                      ((not (positive-integer? n))
                       (error ">>: n is not a positive number"))
                      (t
                       (funcall k (ash number (- n))))))
              (error ">>: invalid arity")))))
          

;; (bitwise-and . numbers)
;; Return a bitwise AND of all provided numbers.
(define "bitwise-and"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (every #'integerp a)
              (funcall k (cl:apply #'logand a))
              (error "bitwise-and: not a number")))))

;; (bitwise-or . numbers)
;; Return a bitwise OR of all provided numbers.
(define "bitwise-or"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (every #'integerp a)
              (funcall k (cl:apply #'logior a))
              (error "bitwise-or: not a number")))))

;; (bitwise-exclusive-or . numbers)
;; Return a bitwise XOR of all provided numbers.
(define "bitwise-exclusive-or"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (every #'integerp a)
              (funcall k (cl:apply #'logxor a))
              (error "bitwise-exclusive-or: not a number")))))

;; (bitwise-not number)
;; Return a bitwise NOT of a number.
(define "bitwise-not"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((number (car a)))
                (if (integerp number)
                    (funcall k (lognot number))
                    (error "bitwise-not: not a number")))
              (error "bitwise-not: invalid arity")))))

;;; Records ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (record? object)
;; Check if object is a record.
(define "record?"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (funcall k (bool->cbool (record? (car a))))
              (error "record?: invalid arity")))))

;; (record type . slots)
;; Create a record with particular type, filling all of the slots.
;; TODO hide?
(define "record"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (cond ((not (>= (length a) 1))
                 (error "record: invalid arity"))
                ((not (array? (car a)))
                 (error "record: invalid type"))
                (t
                 (funcall k (cl:apply #'record a)))))))

;; (record:type record)
;; Get type of a record
(define "record:type"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((record (car a)))
                (if (record? record)
                    (funcall k (record-type (car a)))
                    (error "record:type: not a record")))
              (error "record:type: invalid arity")))))

;; (record:length record)
;; Get length of a record.
(define "record:length"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((record (car a)))
                (if (record record)
                    (funcall k (record-length record))
                    (error "record:length: not a record")))
              (error "record:length: invalid arity")))))

;; (record:nth record n)
;; Get nth field of a record.
(define "record:nth"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 2)
              (destructuring-bind (record n) a
                (if (< -1 n (record-length record))
                    (funcall k (record-nth record n))
                    (error "record:nth: out of bounds")))
              (error "record:nth: invalid arity")))))

;; (record:set! record index value)
;; Set nth field of a record to value.
(define "record:set!"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 3)
              (destructuring-bind (record index value) a
                (cond ((not (record? record))
                       (error "record:set!: not a record"))
                      ((not (positive-integer? index))
                       (error "record:set!: invalid index"))
                      (t
                       (funcall k (record-set! record index value)))))
              (error "record:set!: invalid arity")))))

;;; Environments ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (environment? object)
;; Create a new environment with specified parent environment.
(define "environment?"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (funcall k (bool->cbool (environment? (car a))))
              (error "environment?: invalid arity")))))

;; (environment)
;; (environment parent)
;; Create a new environment with specified parent environment.
(define "environment"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (case (length a)
            (0
             (funcall k (environment (intern "nil"))))
            (1
             (let ((parent (car a)))
               (if (environment? parent)
                   (funcall k (environment parent))
                   (error
                    "environment: parent is not an environment"))))
            (t
             (error "environment: invalid arity"))))))

;; (environment:set! environment symbol value)
;; Set value of a symbol 'symbol' in environment.
(define "environment:set!"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 3)
              (destructuring-bind (environment symbol value) a
                (cond ((not (environment? environment))
                       (error "environment:set!: not an environment"))
                      ((not (symbol? symbol))
                       (error "environment:set!: not a symbol"))
                      (t
                       (funcall k
                                (environment-set! environment
                                                  symbol
                                                  value)))))
              (error "environment:set!: invalid arity")))))

;;; IO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (write-byte byte)
;; Write a byte to standard output.
(define "write-byte"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((byte (car a)))
                (if (byte? byte)
                    (progn (format t "~a" (code-char (car a)))
                           (funcall k (intern "nil")))
                    (error "write-byte: not a byte")))
              (error "write-byte: invalid arity")))))

;; (write string) => nil
;; Write string to standard output.
(define "write"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((string (car a)))
                (if (stringp string)
                    (progn (loop for c across (car a) do (princ c))
                           (funcall k (intern "nil")))
                    (error "write: not a string")))
              (error "write: invalid arity")))))

;; (:print object)
;; A primitive printer. Extended with generics in-langauge.
(define ":print"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (funcall k (print (car a)))
              (error ":print: invalid arity")))))

;;; Questionable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO remove
;; TODO drop continuation properly
;; (die message)
;; Print message and kill current process.
(define "die"
  (wrap (lambda (a e k)
          (declare (ignore e k))
          (error "~a" (car a)))))

;; (environment:find environment symbol)
;; Find an environment in which symbol is bound.
(define "environment:find"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 2)
              (destructuring-bind (environment symbol) a
                (cond ((not (environment? environment))
                       (error "environment:find: not an environment"))
                      ((not (symbol? symbol))
                       (error "environment:find: not a symbol"))
                      (t
                       (let ((e
                               (environment-find environment symbol)))
                         (funcall k (or e (intern "false")))))))
              (error "environment:find: invalid arity")))))

;; (environment::parent environment)
(define "environment::parent"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (if (= (length a) 1)
              (let ((environment (car a)))
                (if (environment? environment)
                    (funcall k (environment-parent environment))
                    (error "not an environment")))
              (error "environment::parent: invalid arity")))))

(define "special::ebind"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (funcall k (special-ebind (car a))))))

(define "special::body"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (funcall k (special-body (car a))))))

(define "special::ptree"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (funcall k (special-ptree (car a))))))

(define "special::environment"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (funcall k (special-environment (car a))))))

(define "special::metadata"
  (wrap (lambda (a e k)
          (declare (ignore e))
          (funcall k (special-metadata (car a))))))
