(in-package #:asdf-user)

(defsystem #:coleslaw-conf
  :name "coleslaw-conf"
  :description "Configuration variable for Coleslaw, Flexible Lisp Blogware"
  :version "0.9.7"
  :license "BSD"
  :author "Brit Butler <redline6561@gmail.com>"
  :pathname "src/"
  :depends-on ()
  :serial t
  :components ((:file "coleslaw-conf")))
