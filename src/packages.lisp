(defpackage :coleslaw
  (:documentation "Homepage: <a href=\"http://github.com/redline6561/coleslaw\">Github</a>")
  (:use :cl)
  (:import-from :alexandria #:hash-table-values
                            #:make-keyword
                            #:mappend
                            #:compose)
  (:import-from :cl-fad #:file-exists-p)
  (:import-from :closure-template #:compile-template)
  (:export #:main
           #:preview
           #:*config*
           #:content
           #:post
           #:index
           #:render-content
           #:add-injection
           ;; The Document Protocol
           #:add-document
           #:find-all
           #:purge-all
           #:discover
           #:publish
           #:page-url
           #:render))
