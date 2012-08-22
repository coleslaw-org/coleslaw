(in-package :coleslaw)

(defclass blog ()
  ((author :initarg :author :initform "" :accessor author)
   (deploy :initarg :deploy :initform nil :accessor deploy)
   (domain :initarg :domain :initform "" :accessor domain)
   (license :initarg :license :initform "CC-BY-SA" :accessor license)
   (plugins :initarg :plugins :initform '() :accessor plugins)
   (repo :initarg :repo :initform #p"/" :accessor repo)
   (sitenav :initarg :sitenav :initform "" :accessor sitenav)
   (staging :initarg :staging :initform #p"/tmp/coleslaw/" :accessor staging)
   (title :initarg :title :initform "" :accessor title)
   (theme :initarg :theme :initform "hyde" :accessor theme)))

(defparameter *config* nil
  "A variable to store the blog configuration and plugin settings.")

(defun load-config (&optional (dir (user-homedir-pathname)))
  "Load the coleslaw configuration from DIR/.coleslawrc. DIR is ~ by default."
  (with-open-file (in (merge-pathnames ".coleslawrc" dir))
    (setf *config* (apply #'make-instance 'blog (read in)))))
