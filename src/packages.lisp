(defpackage :coleslaw
  (:use :cl :closure-template)
  (:import-from :iolib.os #:with-current-directory
                          #:delete-files
                          #:read-symlink
                          #:run-program)
  (:import-from :iolib.pathnames #:file-path-namestring)
  (:export #:main))
