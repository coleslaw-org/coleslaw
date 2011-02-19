(defsystem :coleslaw-dynamic
  :name "coleslaw-dynamic"
  :description "Flexible Lisp Blogware, Dynamic+PostgreSQL edition"
  :version "0.0.1"
  :maintainer "Brit Butler <redline6561@gmail.com>"
  :author "Brit Butler <redline6561@gmail.com>"
  :licence "LLGPL"
  :depends-on (:coleslaw-core :postmodern :restas)
  :components ((:module dynamic
                        :components ((:file "comments")
                                     (:file "admin"
                                            :depends-on ("comments"))))))
