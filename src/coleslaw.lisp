(in-package :coleslaw)

(defun render-page (path html &optional raw)
  "Populate the base template with the provided HTML and write it out to PATH.
If RAW is non-nil, write the content without wrapping it in the base template."
  (let ((filepath (merge-pathnames path (staging *config*))))
    (ensure-directories-exist filepath)
    (with-open-file (out filepath
                         :direction :output
                         :if-does-not-exist :create)
      (let ((content (funcall (theme-fn "BASE")
                              (list :title (title *config*)
                                    :siteroot (domain *config*)
                                    :navigation (sitenav *config*)
                                    :content html
                                    :head-inject (apply #'concatenate 'string
                                                        (gethash :head *injections*))
                                    :body-inject (apply #'concatenate 'string
                                                        (gethash :body *injections*))
                                    :license (license *config*)
                                    :credits (author *config*)))))
        (write-line (if raw html content) out)))))

(defun compile-blog ()
  "Compile the blog to a staging directory in /tmp."
  (let ((staging (staging *config*)))
    ; TODO: More incremental compilation? Don't regen whole blog unnecessarily.
    (when (probe-file staging)
      (delete-directory-and-files staging))
    (ensure-directories-exist staging)
    (with-current-directory staging
      (let ((css-dir (app-path "themes/~a/css" (theme *config*)))
            (static-dir (merge-pathnames "static" (repo *config*))))
        (dolist (dir (list css-dir static-dir))
          (when (probe-file dir)
            (run-program "cp" `("-R" ,(namestring dir) ".")))))
      (render-posts)
      (render-indices)
      (render-feed))
    (deploy staging)))

(defun update-symlink (path target)
  "Update the symlink at PATH to point to TARGET."
  (run-program "ln" (list "-sfn" (namestring target) (namestring path))))

(defgeneric deploy (dir)
  (:documentation "Deploy DIR, updating the .prev and .curr symlinks.")
  (:method (dir)
    (let ((new-build (app-path "generated/~a" (get-universal-time))))
      (ensure-directories-exist new-build)
      (with-current-directory coleslaw-conf:*basedir*
        (run-program "mv" (mapcar #'namestring (list dir new-build)))
        (when (probe-file (app-path ".prev"))
          (delete-directory-and-files (read-symlink (app-path ".prev"))))
        (when (probe-file (app-path ".curr"))
          (update-symlink ".prev" (read-symlink (app-path ".curr"))))
        (update-symlink ".curr" new-build)))))

(defun main ()
  "Load the user's config, then compile and deploy the blog."
  (load-config)
  (compile-theme)
  (compile-blog))
