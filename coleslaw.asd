(defsystem #:coleslaw
  :name "coleslaw-core"
  :description "Flexible Lisp Blogware"
  :version "0.6.5"
  :license "BSD"
  :author "Brit Butler <redline6561@gmail.com>"
  :pathname "src/"
  :depends-on (:closure-template :3bmd :3bmd-ext-code-blocks
               :alexandria :local-time :trivial-shell :cl-fad)
  :serial t
  :components ((:file "packages")
               (:file "util")
               (:file "config")
               (:file "themes")
               (:file "coleslaw")
               (:file "feeds")
               (:file "posts")
               (:file "indices"))
  :in-order-to ((test-op (load-op coleslaw-tests)))
  :perform (test-op :after (op c)
                    (funcall (intern "RUN!" :coleslaw-tests)
                             (intern "COLESLAW-TESTS" :coleslaw-tests))))

(defsystem #:coleslaw-tests
  :depends-on (coleslaw fiveam)
  :pathname "tests/"
  :serial t
  :components ((:file "packages")
               (:file "tests")))

(defmethod operation-done-p ((op test-op)
                             (c (eql (find-system :coleslaw))))
  (values nil))

(defpackage #:coleslaw-conf (:export #:*basedir*))
(defparameter coleslaw-conf:*basedir*
  (make-pathname :name nil :type nil :defaults *load-truename*))
