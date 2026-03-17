(defsystem :centi
  :name "Centi"
  :version "0.0.1"
  :author "German Bichkovich"
  :maintainer "German Bichkovich"
  :description "My very own lisp"
  :depends-on (#:uiop)
  :components ((:module "src"
                :components ((:file "package")
                             (:module "datatypes"
                              :components ((:file "symbol")
                                           (:file "environment")
                                           (:file "record")
                                           (:file "callable")))
                             (:file "reader")
                             (:file "printer")
                             (:file "interpreter")
                             (:file "builtins")
                             (:file "main"))))
  :build-operation "program-op"
  :build-pathname "./bin/centi"
  :entry-point "centi::main")
