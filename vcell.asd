(in-package :asdf-user)

(defsystem vcell
  :author "pheonix9001"
  :description "Powerful dataflow in Lisp"
  :depends-on (trivial-types)
  :components
  ((:module "src"
    :components
    ((:file "main")))))
