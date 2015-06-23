(defpackage #:coleslaw-cli/serve
  (:use #:cl)
  (:import-from #:coleslaw
                #:*config*
                #:discover-config-path
                #:load-config
                #:staging-dir)
  (:import-from #:coleslaw-cli/utils/directory-server
                #:directory-server)
  (:documentation "Serve the rendered blog.")
  (:export
   #:serve))
(in-package #:coleslaw-cli/serve)


(defun serve (&key config blog-dir host (port 8080))
  (let* ((current-dir (uiop/os:getcwd))
         (repo-dir (or blog-dir
                       current-dir))
         (config-file (or config
                          (discover-config-path current-dir))))
    (load-config config-file repo-dir)

    (hunchentoot:start
     (make-instance 'hunchentoot:easy-acceptor
                    :address host
                    :port port ))
    (push (directory-server "/"
                            (staging-dir *config*))
          hunchentoot:*dispatch-table*)
    ;; To prevent the app to return immediately. Should investigate further.
    ;; See:
    ;; http://stackoverflow.com/questions/25797353/hunchentoot-based-app-in-a-lisp-image-from-buildapp-immediately-returns
    (sb-impl::toplevel-repl nil)))

(setf (documentation #'serve 'function)
      (documentation *package* t))
