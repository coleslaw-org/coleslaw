(defsystem :coleslaw-static
  :name "coleslaw-static"
  :description "Flexible Lisp Blogware, Static+S3 edition"
  :version "0.0.1"
  :maintainer "Brit Butler <redline6561@gmail.com>"
  :author "Brit Butler <redline6561@gmail.com>"
  :licence "LLGPL"
  :depends-on (:coleslaw-core :zs3 :trivial-timers)
  :components ((:module static
                        :components ((:file "comments")))))
