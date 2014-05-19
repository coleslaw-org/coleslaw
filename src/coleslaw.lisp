(in-package :coleslaw)

(defvar *last-revision* nil
  "The git revision prior to the last push. For use with GET-UPDATED-FILES.")

(defun main (&optional (repo-dir "") oldrev)
  "Load the user's config file, then compile and deploy the site. Optionally,
REPO-DIR is the location of the blog repo and OLDREV is the revision prior to
the last push."
  (setf *last-revision* oldrev)
  (load-config repo-dir)
  (load-content)
  (compile-theme (theme *config*))
  (let ((dir (staging-dir *config*)))
    (compile-blog dir)
    (deploy dir)))

(defun load-content ()
  "Load all content stored in the blog's repo."
  (do-subclasses (ctype content)
    (discover ctype))
  (update-content-metadata)
  (do-subclasses (itype index)
    (discover itype)))

(defun compile-blog (staging)
  "Compile the blog to a STAGING directory as specified in .coleslawrc."
  (ensure-directories-exist staging)
  (with-current-directory staging
    (dolist (dir (list (app-path "themes/~a/css" (theme *config*))
                       (app-path "themes/~a/img" (theme *config*))
                       (app-path "themes/~a/js" (theme *config*))
                       (merge-pathnames "static" (repo *config*))))
      (when (probe-file dir)
        (run-program "rsync --delete -raz ~a ." dir)))
    (do-subclasses (ctype content)
      (publish ctype))
    (do-subclasses (itype index)
      (publish itype))
    (update-symlink "index.html" "1.html")))

(defgeneric deploy (staging)
  (:documentation "Deploy the STAGING dir, updating the .prev and .curr symlinks.")
  (:method (staging)
    (let* ((dest (deploy-dir *config*))
           (new-build (rel-path dest "generated/~a" (get-universal-time)))
           (prev (rel-path dest ".prev"))
           (curr (rel-path dest ".curr")))
      (ensure-directories-exist new-build)
      (run-program "mv ~a ~a" staging new-build)
      (when (and (probe-file prev) (truename prev))
        (run-program "rm -r ~a" (truename prev)))
      (when (probe-file curr)
        (update-symlink prev (truename curr)))
      (update-symlink curr new-build))))

(defun update-symlink (path target)
  "Update the symlink at PATH to point to TARGET."
  (run-program "ln -sfn ~a ~a" target path))

(defun preview (path &optional (content-type 'post))
  "Render the content at PATH under user's configured repo and save it to
~/tmp.html. Load the user's config and theme if necessary."
  (let ((current-working-directory (cl-fad:pathname-directory-pathname path)))
    (unless *config*
      (load-config (namestring current-working-directory))
      (compile-theme (theme *config*)))
    (let* ((file (rel-path (repo *config*) path))
           (content (construct content-type (read-content file))))
      (write-file "tmp.html" (render-page content)))))

(defun render-page (content &optional theme-fn &rest render-args)
  "Render the given CONTENT to HTML using THEME-FN if supplied.
Additional args to render CONTENT can be passed via RENDER-ARGS."
  (funcall (or theme-fn (theme-fn 'base))
           (list :config *config*
                 :content content
                 :raw (apply 'render content render-args)
                 :pubdate (format-rfc1123-timestring nil (local-time:now))
                 :injections (find-injections content))))
