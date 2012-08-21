(in-package :coleslaw)

(defun app-path (path &rest args)
  "Take a relative PATH and return the corresponding pathname beneath coleslaw.
If ARGS is provided, use (apply 'format nil PATH ARGS) as the value of PATH."
  (merge-pathnames (apply 'format nil path args) coleslaw-conf:*basedir*))

(defun make-keyword (string)
  "Return a keyword matching STRING."
  (intern (string-upcase string) :keyword))

(defun to-pathname (file parent)
  "Convert an iolib file-path back to a pathname."
  (merge-pathnames (file-path-namestring file) parent))

(defmacro do-files ((var path &optional extension) &body body)
  "For each file under PATH, run BODY. If EXTENSION is provided, only run BODY
on files that match the given extension."
  (alexandria:with-gensyms (ext)
    `(mapcar (lambda (,var)
               ,@(if extension
                     `((let ((,ext (pathname-type ,var)))
                         (when (and ,ext (string= ,ext ,extension))
                           ,@body)))
                     `,body))
             (list-directory ,path))))

(defun render-page (path html)
  "Populate the base template with the provided HTML and write it out to PATH."
  (ensure-directories-exist path)
  (with-open-file (out path
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
      (write-line content out))))

(defun compile-blog ()
  "Compile the blog to a staging directory in /tmp."
  (let ((staging #p"/tmp/coleslaw/"))
    ; TODO: More incremental compilation? Don't regen whole blog unnecessarily.
    (when (probe-file staging)
      (delete-directory-and-files staging))
    (ensure-directories-exist staging)
    (with-current-directory staging
      (let ((css-dir (app-path "themes/~a/css/" (theme *config*)))
            (static-dir (merge-pathnames "static/" (repo *config*))))
        (dolist (dir (list css-dir static-dir))
          (when (probe-file dir)
            (run-program "cp" `("-R" ,(namestring dir) ".")))))
      (render-posts)
      (render-indices))
    (deploy staging)
    (setf (last-published) (last-commit))))

(defun update-symlink (name target)
  "Update the symlink NAME to point to TARGET."
  (run-program "ln" (list "-sfn" (namestring target) name)))

(defgeneric deploy (dir)
  (:documentation "Deploy DIR, updating the .prev and .curr symlinks.")
  (:method (dir)
    (let ((new-build (app-path "generated/~a" (get-universal-time))))
      (run-program "mv" (list dir (namestring new-build)))
      (when (probe-file (app-path ".prev"))
        (delete-directory-and-files (read-symlink (app-path ".prev"))))
      (when (probe-file (app-path ".curr"))
        (update-symlink ".prev" (read-symlink (app-path ".curr"))))
      (update-symlink ".curr" new-build))))

(defun main ()
  (load-config)
  (compile-theme)
  (loop do (if (blog-update-p)
               (compile-blog)
               (sleep (interval *config*)))))
