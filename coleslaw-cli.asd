(defsystem #:coleslaw-cli
  :name "coleslaw"
  :description "Flexible Lisp Blogware"
  :version "0.9.7"
  :license "BSD"
  :author "Brit Butler <redline6561@gmail.com>"
  :pathname "cli/"
  :depends-on (:coleslaw
               :clack
               :trivia
               :uiop)
  :serial t
  :components ((:file "cli")))

