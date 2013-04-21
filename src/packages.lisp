(defpackage :coleslaw
  (:documentation "Homepage: <a href=\"http://github.com/redline6561/coleslaw\">Github</a>")
  (:use :cl)
  (:import-from :alexandria #:hash-table-values
                            #:make-keyword
                            #:mappend
                            #:compose)
  (:import-from :closure-template #:compile-template)
  (:import-from :cl-fad #:delete-directory-and-files)
  (:export #:main
           #:preview
           #:*config*
           #:blog
           #:content
           #:post
           #:index
           #:page-path
           #:discover
           #:publish
           #:render
           #:render-content
           #:read-content
           #:add-injection))
