(in-package :coleslaw)

(defgeneric render (content &key &allow-other-keys)
  (:documentation "Render the given CONTENT to HTML."))

(defun render-page (content &optional theme-fn &rest render-args)
  "Render the given CONTENT to disk using THEME-FN if supplied.
Additional args to render CONTENT can be passed via RENDER-ARGS."
  (let* ((path (etypecase content
                 (post (format nil "posts/~a.html" (post-slug post)))
                 (index (index-path index))))
         (filepath (merge-pathnames path (staging *config*)))
         (page (funcall (theme-fn (or theme-fn 'base))
                        (list :config *config*
                              :content (apply 'render content render-args)
                              :body-inject (gethash :body *injections*)
                              :head-inject (gethash :head *injections*)))))
    (ensure-directories-exist filepath)
    (with-open-file (out filepath
                     :direction :output
                     :if-does-not-exist :create)
      (write page :stream out))))

(defun compile-blog (staging)
  "Compile the blog to a STAGING directory as specified in .coleslawrc."
  (when (probe-file staging)
    (run-program "rm -R ~a" staging))
  (ensure-directories-exist staging)
  (with-current-directory staging
    (dolist (dir (list (app-path "themes/~a/css" (theme *config*))
                       (merge-pathnames "static" (repo *config*))))
      (when (probe-file dir)
        (run-program "cp -R ~a ." dir)))
    (render-posts)
    (render-indices)
    (render-feeds)))

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
        (when (probe-file prev)
          (let ((dest (truename prev)))
            (if (equal prev dest)
                (delete-file prev)
                (run-program "rm -R ~a" dest))))
        (when (probe-file curr)
          (update-symlink prev (truename curr)))
        (update-symlink curr new-build)))))

(defun main ()
  "Load the user's config, then compile and deploy the blog."
  (load-config)
  (compile-theme)
  (compile-blog (staging *config*))
  (deploy (staging *config*)))
