;;;; rayngine.asd

(asdf:defsystem #:rayngine
  :description "Describe rayngine here"
  :author "Lonjie <lonjil@gmail.com>"
  :license  "Pain"
  :version "0.0.1"
  :serial t
  :depends-on (#:origin #:zpng)
  :components ((:file "package")
               (:file "stack")
               (:file "structures")
               (:file "sdf")
               (:file "rayngine")))
