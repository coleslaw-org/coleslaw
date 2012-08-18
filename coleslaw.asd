(defsystem #:coleslaw
  :name "coleslaw-core"
  :description "Flexible Lisp Blogware"
  :version "0.0.3"
  :license "BSD"
  :author "Brit Butler <redline6561@gmail.com>"
  :pathname "src/"
  :depends-on (:closure-template :iolib.os :local-time :alexandria)
  :serial t
  :components ((:file "packages")
               (:file "coleslaw")
               (:file "themes")
               (:file "posts")
               (:file "indices")
               (:file "plugins"))
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
