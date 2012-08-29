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
    (let ((css-dir (app-path "themes/~a/css" (theme *config*)))
          (static-dir (merge-pathnames "static" (repo *config*))))
      (dolist (dir (list css-dir static-dir))
        (when (probe-file dir)
          (shell-command (format nil "cp -R ~a ." dir)))))
    (render-posts)
    (render-indices)
    (render-feed)))

(defun update-symlink (path target)
  "Update the symlink at PATH to point to TARGET."
  (shell-command (format nil "ln -sfn ~a ~a" target path)))

(defgeneric deploy (staging)
  (:documentation "Deploy the STAGING dir, updating the .prev and .curr symlinks.")
  (:method (staging)
    (flet ((deploy-path (path &rest args)
             (merge-pathnames (apply 'format nil path args) (deploy *config*))))
      (let ((new-build (deploy-path "generated/~a" (get-universal-time)))
            (prev (deploy-path ".prev"))
            (curr (deploy-path ".curr")))
        (ensure-directories-exist new-build)
        (with-current-directory coleslaw-conf:*basedir*
          (shell-command (format nil "mv ~a ~a" staging new-build))
          (if (and (probe-file prev) (equal prev (truename prev)))
              (delete-file prev)
              (cl-fad:delete-directory-and-files (truename prev)))
          (when (probe-file curr)
            (update-symlink prev (truename curr)))
          (update-symlink curr new-build))))))

(defun main ()
  "Load the user's config, then compile and deploy the blog."
  (load-config)
  (compile-theme)
  (compile-blog (staging *config*))
  (deploy (staging *config*)))
