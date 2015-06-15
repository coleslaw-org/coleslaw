(defsystem #:coleslaw-cli
  :name "coleslaw-cli"
  :description "CLI tools for Coleslaw"
  :version (:read-file-from "version.lisp-expr")
  :license "BSD"
  :author "Javier Olaechea <pirata@gmail.com"
  :pathname "src/"
  :serial t
  :depends-on (#:coleslaw)
  :components ((:file "cli")))
