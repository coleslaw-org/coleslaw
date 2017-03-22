(defpackage :coleslaw
  (:documentation "Homepage: <a href=\"http://github.com/redline6561/coleslaw\">Github</a>")
  (:use :cl)
  (:import-from :alexandria #:hash-table-values
                            #:make-keyword
                            #:mappend)
  (:import-from :cl-fad #:file-exists-p)
  (:import-from :cl-ppcre #:scan-to-strings #:split)
  (:import-from :closure-template #:compile-template)
  (:import-from :local-time #:format-rfc1123-timestring)
  (:import-from :uiop #:getcwd
                      #:ensure-directory-pathname)
  (:export #:main
           #:preview
           #:*config*
           ;; Config Accessors
           #:author
           #:deploy-dir
           #:domain
           #:page-ext
           #:repo-dir
           #:staging-dir
           #:title
           ;; Core Classes
           #:content
           #:post
           #:index
           ;; Content Helpers
           #:title-of
           #:author-of
           #:find-content-by-path
           ;; Theming + Plugin API
           #:theme-fn
           #:plugin-conf-error
           #:render-text
           #:add-injection
           #:get-updated-files
           #:deploy
           ;; The Document Protocol
           #:discover
           #:publish
           #:page-url
           #:render
           #:find-all
           #:purge-all
           #:add-document
           #:delete-document
           #:write-document
           #:content-text

	   ;; Error reporting
           #:assert-field))
