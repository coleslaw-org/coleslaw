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

(defparameter *config* nil
  "A variable to store the blog configuration and plugin settings.")

(defun app-path (path)
  "Take a relative PATH and return the corresponding pathname beneath coleslaw."
  (merge-pathnames path coleslaw-conf:*basedir*))

(defun load-config ()
  nil)

(defun exit-handler ()
  nil)

(defun compile-blog ()
  (let ((staging #p"/tmp/coleslaw/"))
    ; TODO: More incremental compilation? Don't regen whole blog unnecessarily.
    (if (probe-file staging)
        (iolib.os:delete-files staging :recursive t)
        (ensure-directories-exist staging))
    (with-current-directory staging
      (let ((css-dir (app-path (format nil "themes/~a/css/" (theme *config*))))
            (static-dir (merge-pathnames "static/" (repo *config*))))
        (dolist (dir (list css-dir static-dir))
          (iolib.os:run-program "cp" `("-R" ,dir "."))))
      (render-posts)
      (render-indices))
    (deploy staging)))

(defun main ()
  (load-config)
  (unwind-protect
       (loop do (if (blog-update-p)
                    (compile-blog)
                    (sleep (interval *config*))))
    (exit-handler)))
