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

(defun compile-blog (staging)
  "Compile the blog to a STAGING directory as specified in .coleslawrc."
  ; TODO: More incremental compilation? Don't regen whole blog unnecessarily.
  (when (probe-file staging)
    (cl-fad:delete-directory-and-files staging))
  (ensure-directories-exist staging)
  (with-current-directory staging
    (dolist (dir (list (app-path "themes/~a/css" (theme *config*))
                       (merge-pathnames "static" (repo *config*))))
      (when (probe-file dir)
        (run-program "cp -R ~a ." dir)))
    (render-posts)
    (render-indices)
    (render-feed)))

(defgeneric deploy (staging)
  (:documentation "Deploy the STAGING dir, updating the .prev and .curr symlinks.")
  (:method (staging)
    (with-current-directory coleslaw-conf:*basedir*
      (let* ((coleslaw-conf:*basedir* (deploy *config*))
             (new-build (app-path "generated/~a" (get-universal-time)))
             (prev (app-path ".prev"))
             (curr (app-path ".curr")))
        (ensure-directories-exist new-build)
        (run-program "mv ~a ~a" staging new-build)
        (if (and (probe-file prev) (equal prev (truename prev)))
            (delete-file prev)
            (cl-fad:delete-directory-and-files (truename prev)))
        (when (probe-file curr)
          (update-symlink prev (truename curr)))
        (update-symlink curr new-build)))))

(defun main ()
  "Load the user's config, then compile and deploy the blog."
  (load-config)
  (compile-theme)
  (compile-blog (staging *config*))
  (deploy (staging *config*)))
