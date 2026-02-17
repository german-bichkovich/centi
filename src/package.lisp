(defpackage centi
  (:use :cl)
  (:shadow
   :symbol
   :symbol-name
   :intern
   :special
   :function
   :read
   :read-string
   :print
   :eval
   :load
   :apply)
  (:export

   ;; Symbols
   :symbol
   :symbol?
   :symbol-name
   :intern
   :interned?

   ;; Callables
   :special
   :special?
   :special-body
   :special-environment
   :special-ptree
   :special-ebind ; TODO rename to full names
   :special-new
   :function?
   :function
   :wrap
   :unwrap
   :generic?

   ;; Environments
   :environment
   :environment?
   :environment-parent
   :environment-bindings ; TODO
   :environment-get
   :environment-set!
   :environment-in?
   :environment-empty?
   :environment-find

   ;; Records
   :record
   :record?
   :record-length
   :record-type
   :record-get
   :record-set!
   
   ;; Reader
   :read
   :read-string

   ;; Printer
   :print

   ;; Interpreter
   :destructure
   :eval ; TODO evaluate?
   :eval-many
   :apply

   :load
   :main))
