(defpackage :coleslaw
  (:documentation "Homepage: <a href=\"http://github.com/redline6561/coleslaw\">Github</a>")
  (:use :cl)
  (:import-from :alexandria #:hash-table-values
                            #:make-keyword
                            #:mappend)
  (:import-from :closure-template #:compile-template)
  (:export #:main
           #:blog
           #:content
           #:post
           #:index
           #:page-path
           #:discover
           #:publish
           #:render
           #:render-content
           #:add-injection))
