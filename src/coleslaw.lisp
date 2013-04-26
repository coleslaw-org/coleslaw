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

(defgeneric page-url (object)
  (:documentation "The url to the object, without the domain"))

(defmethod page-url :around ((object t))
  (let ((result (call-next-method)))
    (namestring (if (pathname-type result)
                  result
                  (make-pathname :type "html" :defaults result)))))

(defun page-path (object)
  "The path to store OBJECT at once rendered."
  (rel-path (staging-dir *config*) (page-url object)))

(defun render-page (content &optional theme-fn &rest render-args)
  "Render the given CONTENT to disk using THEME-FN if supplied.
Additional args to render CONTENT can be passed via RENDER-ARGS."
  (funcall (or theme-fn (theme-fn 'base))
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
                   :if-exists :supersede
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
    (do-ctypes (publish ctype))
    (render-indices)
    (render-feeds (feeds *config*))))

(defgeneric deploy (staging)
  (:documentation "Deploy the STAGING dir, updating the .prev and .curr symlinks.")
  (:method (staging)
    (let* ((dest (deploy-dir *config*))
           (new-build (rel-path dest "generated/~a" (get-universal-time)))
           (prev (rel-path dest ".prev"))
           (curr (rel-path dest ".curr")))
      (ensure-directories-exist new-build)
      (run-program "mv ~a ~a" staging new-build)
      (when (probe-file prev)
        (delete-directory-and-files (truename prev) :if-does-not-exist :ignore))
      (when (probe-file curr)
        (update-symlink prev (truename curr)))
      (update-symlink curr new-build))))

(defun main (config-key)
  "Load the user's config section corresponding to CONFIG-KEY, then
compile and deploy the blog."
  (load-config config-key)
  (load-content)
  (compile-theme (theme *config*))
  (let ((blog (staging-dir *config*)))
    (compile-blog blog)
    (deploy blog)))

(defun preview (path &optional (content-type 'post))
  "Render the content at PATH under user's configured repo and save it to
~/tmp.html. Load the user's config and theme if necessary."
  (unless *config*
    (load-config nil)
    (compile-theme (theme *config*)))
  (let* ((file (rel-path (repo *config*) path))
         (content (construct content-type (read-content file))))
    (write-page "~/tmp.html" (render-page content))))
