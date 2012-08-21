(defpackage :coleslaw
  (:use :cl :closure-template)
  (:import-from :cl-fad #:delete-directory-and-files
                        #:list-directory)
  (:import-from :iolib.os #:with-current-directory
                          #:read-symlink
                          #:run-program)
  (:import-from :iolib.pathnames #:file-path-namestring)
  (:import-from :alexandria #:hash-table-values)
  (:export #:main
           #:add-injection
           #:render-content
           #:deploy))
