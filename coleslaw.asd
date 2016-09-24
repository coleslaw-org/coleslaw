(in-package #:asdf-user)

(defsystem #:coleslaw
  :name "coleslaw"
  :description "Flexible Lisp Blogware"
  :version "0.9.7"
  :license "BSD"
  :author "Brit Butler <redline6561@gmail.com>"
  :pathname "src/"
  :depends-on (:closure-template
               :3bmd
               :3bmd-ext-code-blocks
               :alexandria
               :local-time
               :inferior-shell
               :cl-fad
               :cl-ppcre
               :closer-mop
               :cl-unicode
               :uiop)
  :serial t
  :components ((:file "coleslaw-conf")
               (:file "packages")
               (:file "util")
               (:file "config")
               (:file "themes")
               (:file "documents")
               (:file "content")
               (:file "posts")
               (:file "indexes")
               (:file "feeds")
               (:file "coleslaw"))
  :in-order-to ((test-op (test-op coleslaw-test))))
