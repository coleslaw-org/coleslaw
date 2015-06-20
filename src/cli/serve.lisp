(defpackage #:coleslaw-cli/serve
  (:use #:cl)
  (:import-from #:coleslaw
                #:*config*
                #:discover-config-path
                #:load-config
                #:staging-dir)
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
          hunchentoot:*dispatch-table*)
    ;; To prevent the app to return immediately. Should investigate further.
    ;; See:
    ;; http://stackoverflow.com/questions/25797353/hunchentoot-based-app-in-a-lisp-image-from-buildapp-immediately-returns
    #+sbcl(sb-impl::toplevel-repl nil)))

(setf (documentation #'serve 'function)
      (documentation *package* t))
