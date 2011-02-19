(defsystem :coleslaw-core
  :name "coleslaw-core"
  :description "Flexible Lisp Blogware, Core utilities"
  :version "0.0.1"
  :maintainer "Brit Butler <redline6561@gmail.com>"
  :author "Brit Butler <redline6561@gmail.com>"
  :licence "LLGPL"
  :depends-on (:cl-markdown :docutils :closure-template
               :external-program :local-time) ; parenscript?
  :components ((:module core
                        :components ((:file "packages")
                                     (:file "coleslaw"
                                            :depends-on ("packages"))
                                     (:file "storage"
                                            :depends-on ("coleslaw"))
                                     (:file "posts"
                                            :depends-on ("storage"))
                                     (:file "indices"
                                            :depends-on ("posts"))))))
