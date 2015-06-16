(defpackage #:coleslaw-cli/clean
  (:use #:cl)
  (:documentation "Deletes the generate static files.")
  (:export
   #:clean))

(in-package #:coleslaw-cli/clean)

(defun clean (&key blog-dir config)
  (let* ((current-dir (uiop/os:getcwd))
         (blog-dir (or blog-dir current-dir))
         (config-file (or config
                          (discover-config-path current-dir))))
    (dolist (dir (list (staging-dir *config*)
                       (deploy-dir *config*)))
      (uiop/filesystem:delete-directory-tree (pathname dir)
                                             :if-does-not-exist :ignore
                                             :validate t))))
(setf (documentation #'clean 'function)
      (documentation *package* t))
