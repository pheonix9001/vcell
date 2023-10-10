(in-package :asdf-user)

(defsystem wcells
  :author "pheonix9001"
  :description "Superior concurrency with dataflow"
  :depends-on (trivial-types)
  :components
  ((:module "src"
    :components
    ((:file "main")))))
