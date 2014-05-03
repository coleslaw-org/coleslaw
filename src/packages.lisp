(defpackage :coleslaw
  (:documentation "Homepage: <a href=\"http://github.com/redline6561/coleslaw\">Github</a>")
  (:use :cl)
  (:import-from :alexandria #:hash-table-values
                            #:make-keyword
                            #:mappend
                            #:compose)
  (:import-from :cl-fad #:file-exists-p)
  (:import-from :closure-template #:compile-template)
  (:import-from :local-time #:format-rfc1123-timestring)
  (:export #:main
           #:preview
           #:*config*
           #:*updated-files*
           #:content
           #:post
           #:index
           #:render-text
           #:add-injection
           #:theme-fn
           ;; The Document Protocol
           #:add-document
           #:find-all
           #:purge-all
           #:discover
           #:publish
           #:page-url
           #:render
           #:write-document))
