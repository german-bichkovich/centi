(defpackage centi.datatypes.symbol
  (:use :cl)
  (:shadow
   :intern
   :symbol)
  (:export
   :intern
   :new
   :name
   :interned?
   :symbol?))

(defpackage centi.datatypes.environment
  (:use :cl)
  (:local-nicknames (:s :centi.datatypes.symbol))
  (:shadow
   :get
   :find)
  (:export
   :environment?
   :new
   :parent
   :bindings
   :get
   :set!
   :contains?
   :empty?
   :find))

(defpackage centi.datatypes.record
  (:use :cl)
  (:shadow
   :get
   :length)
  (:export
   :record?
   :length
   :record
   :get
   :set!))

(defpackage centi.datatypes.hashmap
  (:use :cl)
  (:local-nicknames (:s :centi.datatypes.symbol))
  (:shadow
   :get
   :find)
  (:export
   :new
   :hashmap?
   :each
   :contains?
   :copy
   :find
   :get
   :set!
   :capacity))

(defpackage centi.datatypes.callable
  (:use :cl)
  (:shadow :function :special)
  (:local-nicknames (:r :centi.datatypes.record)
                    (:s :centi.datatypes.symbol))
  (:export
   :make-special
   :special?
   :body
   :environment
   :ptree
   :ebind
   :function?
   :wrap
   :unwrap
   :generic?))

;; TODO fill ot delete
(defpackage centi.utils
  (:use :cl)
  (:export))

(defpackage centi.reader
  (:use :cl)
  (:local-nicknames (:s :centi.datatypes.symbol))
  (:shadow
   :read)
  (:export
   :read
   :read-string))

(defpackage centi.printer
  (:use :cl)
  (:local-nicknames (:c :centi.datatypes.callable)
                    (:r :centi.datatypes.record)
                    (:s :centi.datatypes.symbol))
  (:shadow
   :print
   :print1)
  (:export
   :print
   :print1))

(defpackage centi.interpreter
  (:use :cl)
  (:local-nicknames (:s :centi.datatypes.symbol)
                    (:c :centi.datatypes.callable)
                    (:e :centi.datatypes.environment))
  (:shadow
   :eval
   :apply)
  (:export
   :destructure
   :eval
   :eval-many
   :apply))

(defpackage centi.builtins
  (:use :cl)
  (:local-nicknames (:i :centi.interpreter)
                    (:p :centi.printer)
                    (:s :centi.datatypes.symbol)
                    (:c :centi.datatypes.callable)
                    (:r :centi.datatypes.record)
                    (:e :centi.datatypes.environment)
                    (:h :centi.datatypes.hashmap))
  (:shadow
   :special
   :function)
  (:export :*stdenv*))

(defpackage centi.vm
  (:use :cl))

(defpackage centi.assembler
  (:use :cl))

(defpackage centi.compiler
  (:use :cl))

(defpackage centi
  (:use :cl)
  (:local-nicknames (:r :centi.reader)
                    (:p :centi.printer)
                    (:i :centi.interpreter)
                    (:b :centi.builtins)
                    (:s :centi.datatypes.symbol))
  (:shadow
   :load)
  (:export :main
           :load
           :repl))
