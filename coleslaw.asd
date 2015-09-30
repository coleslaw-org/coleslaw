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
               :cl-unicode)
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "config")
               (:file "themes")
               (:file "documents")
               (:file "content")
               (:file "indexes")
               (:file "feeds")
               (:file "coleslaw"))
  :in-order-to ((test-op (test-op coleslaw-test))))

(defsystem #:coleslaw-test
  :description "A test suite for coleslaw."
  :license "BSD"
  :author "Brit Butler <redline6561@gmail.com>"
  :depends-on (:coleslaw :prove)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "tests"
                :components
                        ((:test-file "tests"))))
  :perform (test-op :after (op c)
                    (uiop:symbol-call :prove 'run c)))

(defpackage #:coleslaw-conf (:export #:*basedir*))
(defparameter coleslaw-conf:*basedir*
  (make-pathname :name nil :type nil :defaults *load-truename*))
