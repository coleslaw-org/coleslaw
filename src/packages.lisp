(defpackage :coleslaw
  (:documentation "Homepage: <a href=\"http://github.com/redline6561/coleslaw\">Github</a>")
  (:use :cl :closure-template)
  (:import-from :trivial-shell #:shell-command)
  (:import-from :iolib.os #:with-current-directory)
  (:import-from :alexandria #:hash-table-values
                            #:make-keyword)
  (:export #:main
           #:add-injection
           #:render-content
           #:render-feed
           #:deploy))
