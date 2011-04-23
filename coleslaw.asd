(defsystem :coleslaw
  :name "coleslaw-core"
  :description "Flexible Lisp Blogware"
  :version "0.0.2"
  :maintainer "Brit Butler <redline6561@gmail.com>"
  :author "Brit Butler <redline6561@gmail.com>"
  :licence "LLGPL"
  :depends-on (:cl-markdown :docutils :closure-template
               :cl-fad :local-time)
  :components ((:module src
                        :components ((:file "packages")
                                     (:file "coleslaw"
                                            :depends-on ("packages"))
                                     (:file "themes"
                                            :depends-on ("coleslaw"))
                                     (:file "comments"
                                            :depends-on ("coleslaw"))
                                     (:file "posts"
                                            :depends-on ("coleslaw"))
                                     (:file "indices"
                                            :depends-on ("posts"))
                                     (:file "plugins"
                                            :depends-on ("packages"))))))
