(defpackage :coleslaw
  (:documentation "Homepage: <a href=\"http://github.com/redline6561/coleslaw\">Github</a>")
  (:use :cl)
  (:import-from :alexandria #:hash-table-values
                            #:make-keyword
                            #:mappend)
  (:import-from :cl-fad #:file-exists-p)
  (:import-from :cl-ppcre #:scan-to-strings)
  (:import-from :closure-template #:compile-template)
  (:import-from :local-time #:format-rfc1123-timestring)
  (:import-from :uiop #:getcwd
                      #:ensure-directory-pathname)
  (:export #:main
           #:preview
           #:*config*
           ;; Core Classes
           #:content
           #:post
           #:index
           ;; Ctype Registry
           #:*ctype-registry*
           #:ensure-ctype-record
           #:get-ctype-attributes
           #:ctype-indexable-p
           #:set-ctype-indexable
           #:indexable-ctypes
           #:register-ctype-superclass
           #:register-superclass-to-ctypes
           #:ensure-ctype
           #:ensure-ctypes
           #:defcontent
           #:defmodifier
           ;; Content Helpers
           #:title-of
           #:author-of
           #:find-content-by-path
           ;; Theming + Plugin API
           #:theme-fn
           #:ensure-plugin
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
           #:content-text))
