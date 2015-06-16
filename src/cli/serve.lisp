(defpackage #:coleslaw-cli/serve
  (:use #:cl)
  (:documentation "Serve the rendered blog.")
  (:export
   #:serve))
(in-package #:coleslaw-cli/serve)

(defun serve (&key config blog-dir host port)
  (let* ((current-dir (uiop/os:getcwd))
         (repo-dir (or blog-dir
                       current-dir))
         (config-file (or config
                          (discover-config-path current-dir))))
    (load-config config-file repo-dir)
    #+nil(compile-blog (staging-dir *config*)) ; Should the blog be compiled?

    (hunchentoot:start
     (make-instance 'hunchentoot:easy-acceptor
                    :port 4242 ))
    (push (hunchentoot:create-folder-dispatcher-and-handler "/"
                                                            (staging-dir *config*))
          hunchentoot:*dispatch-table*)))

(setf (documentation #'serve 'function)
      (documentation *package* t))
