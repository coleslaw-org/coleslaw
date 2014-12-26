#!/usr/bin/cl -Q -s hunchentoot -sp coleslaw --entry serve

(defun serve (argv)
  (let* ((parameters (coleslaw-cli:process-parameters argv))
         (current-dir (uiop/os:getcwd))
         (repo-dir (or (cdr (assoc :repo-dir parameters))
                       current-dir))
         (config-file (or (cdr (assoc :config parameters))
                          (discover-config-path current-dir))))
    (load-config config-file repo-dir)
    #+nil(compile-blog (staging-dir *config*)) ; Should the blog be compiled?

    (hunchentoot:start
     (make-instance 'hunchentoot:easy-acceptor
                    :port 4242 ))
    (push (hunchentoot:create-folder-dispatcher-and-handler "/"
                                                            (staging-dir *config*))
          hunchentoot:*dispatch-table*)))
