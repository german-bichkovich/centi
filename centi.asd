(defsystem :centi
  :name "Centi"
  :version "0.0.1"
  :author "Geri"
  :maintainer "Geri"
  :description "My very own lisp"
  :depends-on (#:uiop)
  :components ((:module "src"
                :components ((:file "package")
                             (:module "datatypes"
                              :components ((:file "symbol")
                                           (:file "environment")
                                           (:file "callable")
                                           (:file "record")
                                           (:file "hashmap")))
                             (:file "reader")
                             (:file "printer")
                             (:file "interpreter")
                             (:file "builtins")
                             (:file "vm")
                             (:file "assembler")
                             (:file "compiler")
                             (:file "main")))))
