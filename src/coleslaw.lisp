(in-package :coleslaw)

(defclass blog ()
  ((author :initarg :author :initform "" :accessor author)
   (domain :initarg :domain :initform "" :accessor domain)
   (interval :initarg :interval :initform 600 :accessor interval)
   (license :initarg :license :initform "" :accessor license)
   (plugins :initarg :plugins :initform '() :accessor plugins)
   (repo :initarg :repo :initform #p"/" :accessor repo)
   (sitenav :initarg :sitenav :initform "" :accessor sitenav)
   (title :initarg :title :initform "" :accessor title)
   (theme :initarg :theme :initform "hyde" :accessor theme)))

(defun app-path (path)
  "Take a relative PATH and return the corresponding pathname beneath coleslaw."
  (merge-pathnames path coleslaw-conf:*basedir*))

(defun compile-blog ()
  (let ((staging #p"/tmp/coleslaw/"))
    ; TODO: More incremental compilation? Don't regen whole blog unnecessarily.
    (if (probe-file staging)
        (delete-files staging :recursive t)
        (ensure-directories-exist staging))
    (with-current-directory staging
      (let ((css-dir (app-path (format nil "themes/~a/css/" (theme *config*))))
            (static-dir (merge-pathnames "static/" (repo *config*))))
        (dolist (dir (list css-dir static-dir))
          (run-program "cp" `("-R" ,dir "."))))
      (render-posts)
      (render-indices))
    (deploy staging)))

(defun deploy (dir)
  "Deploy DIR, updating the .prev and .curr symlinks."
  (let ((new-build (namestring (app-path (format nil "generated/~a"
                                                 (get-universal-time))))))
    (run-program "mv" (list dir new-build))
    (when (probe-file (app-path ".prev"))
      (delete-files (read-symlink (app-path ".prev")) :recursive t))
    (when (probe-file (app-path ".curr"))
      (let ((curr-build (namestring (read-symlink (app-path ".curr")))))
        (run-program "ln" (list "-sfn" curr-build ".prev"))))
    (run-program "ln" (list "-sfn" new-build ".curr")))
  (setf (last-published) (last-commit)))

(defun main ()
  (load-config)
  (loop do (if (blog-update-p)
               (compile-blog)
               (sleep (interval *config*)))))
