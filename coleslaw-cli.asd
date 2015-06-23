(in-package #:cl-user)
(defpackage #:coleslaw-cli-system
  (:use #:cl #:asdf))
(in-package #:coleslaw-cli-system)

(defsystem #:coleslaw-cli
  :name "coleslaw-cli"
  :description "CLI tools for Coleslaw"
  :version (:read-file-form "version.lisp-expr")
  :license "BSD"
  :author "Javier Olaechea <pirata@gmail.com"
  :pathname "src/"
  :serial t
  :depends-on (#:alexandria
               #:coleslaw
               #:command-line-arguments
               #:hunchentoot)
  :components
  ((:module "cli"
    :components ((:module "utils"
                  :components ((:file "directory-server")))
                 (:file "build")
                 (:file "clean")
                 (:file "rebuild" :depends-on ("clean" "build"))
                 (:file "serve")
                 (:file "entry" :depends-on ("build"
                                             "clean"
                                             "rebuild"
                                             "serve"))))))
