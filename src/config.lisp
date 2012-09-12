(in-package :coleslaw)

(defclass blog ()
  ((author :initarg :author :initform "" :accessor author)
   (deploy :initarg :deploy :initform nil :accessor deploy)
   (domain :initarg :domain :initform "" :accessor domain)
   (feeds :initarg :feeds :initform nil :accessor feeds)
   (license :initarg :license :initform "CC-BY-SA" :accessor license)
   (plugins :initarg :plugins :initform '() :accessor plugins)
   (repo :initarg :repo :initform #p"/" :accessor repo)
   (sitenav :initarg :sitenav :initform "" :accessor sitenav)
   (staging :initarg :staging :initform #p"/tmp/coleslaw/" :accessor staging)
   (title :initarg :title :initform "" :accessor title)
   (theme :initarg :theme :initform "hyde" :accessor theme)))

(defparameter *config* nil
  "A variable to store the blog configuration and plugin settings.")

(defun load-plugins (plugins)
  "Resolve the path of each symbol in PLUGINS and call LOAD on the
resulting pathnames. It is expected that the matching *.lisp files
are in the plugins folder in coleslaw's source directory."
  (let ((files (mapcar (lambda (sym)
                         (app-path "plugins/~a" (string-downcase (symbol-name sym))))
                       plugins)))
    (map nil (lambda (file)
               (compile-file file)
               (load file)) files)))

(defun load-config (&optional (dir (user-homedir-pathname)))
  "Load the coleslaw configuration from DIR/.coleslawrc. DIR is ~ by default."
  (with-open-file (in (merge-pathnames ".coleslawrc" dir))
    (setf *config* (apply #'make-instance 'blog (read in))))
  (load-plugins (plugins *config*)))
