(defpackage :coleslaw
  (:documentation "Homepage: <a href=\"http://github.com/redline6561/coleslaw\">Github</a>")
  (:use :cl :closure-template)
  (:import-from :iolib.os #:with-current-directory
                          #:run-program)
  (:import-from :alexandria #:hash-table-values
                            #:make-keyword)
  (:export #:main
           #:add-injection
           #:render-content
           #:render-feed
           #:deploy))
