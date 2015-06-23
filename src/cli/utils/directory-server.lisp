(defpackage #:coleslaw-cli/utils/directory-server
  (:use #:cl)
  (:import-from #:hunchentoot
                #:*request*
                #:+http-forbidden+
                #:request-pathname
                #:abort-request-handler
                #:create-prefix-dispatcher
                #:handle-static-file
                #:parameter-error
                #:return-code*)
  (:import-from #:uiop/pathname
                #:directory-pathname-p)
  (:import-from #:uiop/filesystem
                #:directory-exists-p
                #:file-exists-p)
  (:documentation "Serve the contents of a directory. Works like
  hunchentoot:create-folder-dispatcher-and-handler with the exception that
  instead of listing the directory contents, first it tries to serve the index.html on the directory.")
  (:export
   #:directory-server))

(in-package #:coleslaw-cli/utils/directory-server)

(defun directory-server (uri-prefix base-path)

  (unless (and (stringp uri-prefix)
               (plusp (length uri-prefix))
               (char= (char uri-prefix (1- (length uri-prefix))) #\/))
    (parameter-error "~S must be string ending with a slash." uri-prefix))
  (unless (directory-pathname-p base-path)
    (parameter-error "~S is supposed to denote a directory." base-path))
  
  ;; The handler checks if the uri ends in /, if so search for a file named
  ;; index.html in that path. If it exists serve it, else serve the directory
  ;; contents. If it uri doesn't end if / serve the file always.
  (flet ((handler ()
           (let ((request-path (request-pathname *request* uri-prefix)))
             (if (null request-path)
                 (abort-request-handler)
                 (let* ((absolute-path (merge-pathnames request-path base-path))
                        (index-file (merge-pathnames #P"index.html" absolute-path)))
                   (cond
                     ((file-exists-p absolute-path) (handle-static-file absolute-path))
                     ((and (directory-exists-p absolute-path)
                           (file-exists-p index-file))
                      (handle-static-file index-file))
                     (t (handle-static-file absolute-path))))))))
    (create-prefix-dispatcher uri-prefix #'handler)))
