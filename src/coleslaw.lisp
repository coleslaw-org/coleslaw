(in-package :coleslaw)

(defgeneric render (object &key &allow-other-keys)
  (:documentation "Render the given OBJECT to HTML."))

(defgeneric render-content (text format)
  (:documentation "Compile TEXT from the given FORMAT to HTML for display.")
  (:method (text (format (eql :html)))
    text)
  (:method (text (format (eql :md)))
    (let ((3bmd-code-blocks:*code-blocks* t))
      (with-output-to-string (str)
        (3bmd:parse-string-and-print-to-stream text str)))))

(defgeneric page-path (object)
  (:documentation "The path to store OBJECT at once rendered."))

(defmethod page-path :around ((object t))
  (let ((result (call-next-method)))
    (if (pathname-type result)
        result
        (make-pathname :type "html" :defaults result))))

(defun render-page (content &optional theme-fn &rest render-args)
  "Render the given CONTENT to disk using THEME-FN if supplied.
Additional args to render CONTENT can be passed via RENDER-ARGS."
  (funcall (theme-fn (or theme-fn 'base))
           (list :config *config*
                 :content content
                 :raw (apply 'render content render-args)
                 :pubdate (make-pubdate)
                 :injections (find-injections content))))

(defun write-page (filepath page)
  "Write the given PAGE to FILEPATH."
  (ensure-directories-exist filepath)
  (with-open-file (out filepath
                   :direction :output
                   :if-does-not-exist :create)
    (write-line page out)))

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
    (render-feeds (feeds *config*))))

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
  (load-posts)
  (compile-theme (theme *config*))
  (compile-blog (staging *config*))
  (deploy (staging *config*)))
