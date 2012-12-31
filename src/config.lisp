(in-package :coleslaw)

(defclass blog ()
  ((author :initarg :author :initform "" :accessor author)
   (deploy :initarg :deploy :initform nil :accessor deploy)
   (domain :initarg :domain :initform "" :accessor domain)
   (feeds :initarg :feeds :initform nil :accessor feeds)
   (license :initarg :license :initform nil :accessor license)
   (plugins :initarg :plugins :initform nil :accessor plugins)
   (repo :initarg :repo :initform #p"/" :accessor repo)
   (sitenav :initarg :sitenav :initform nil :accessor sitenav)
   (staging :initarg :staging :initform #p"/tmp/coleslaw/" :accessor staging)
   (title :initarg :title :initform "" :accessor title)
   (theme :initarg :theme :initform "hyde" :accessor theme)))

(define-condition unknown-config-section-error (error)
  ((text :initarg :text :reader text)))

(defparameter *config* nil
  "A variable to store the blog configuration and plugin settings.")

(defun enable-plugin (file &rest args)
  "Given a path to a plugin, FILE, compile+load it, then call its ENABLE function."
  (compile-file file)
  (load file)
  (let* ((pkgname (format nil "coleslaw-~a" (pathname-name file)))
         (plugin-pkg (find-package (string-upcase pkgname))))
    (apply (find-symbol "ENABLE" plugin-pkg) args)))

(defun load-plugins (plugins)
  "Compile and load the listed PLUGINS. It is expected that matching *.lisp files
are in the plugins folder in coleslaw's source directory."
  (flet ((plugin-path (sym)
           (app-path "plugins/~a" (string-downcase (symbol-name sym)))))
    (dolist (plugin plugins)
      (etypecase plugin
        (list (destructuring-bind (name &rest args) plugin
                (apply 'enable-plugin (plugin-path name) args)))
        (symbol (enable-plugin (plugin-path plugin)))))))

(defun load-config (config-key &optional (dir (user-homedir-pathname)))
  "Load the coleslaw configuration for CONFIG-KEY from DIR/.coleslawrc. DIR is ~ by default."
  (with-open-file (in (merge-pathnames ".coleslawrc" dir))
    (let ((config-form (read in)))
      (if (symbolp (car config-form))
          ;; Single site config: ignore CONFIG-KEY.
          (setf *config* (apply #'make-instance 'blog config-form))
          ;; Multi-site config: load config section for CONFIG-KEY.
          (let* ((config-key-pathname (cl-fad:pathname-as-directory config-key))
                 (section (assoc config-key-pathname config-form
                                 :key #'(lambda (str) (cl-fad:pathname-as-directory str))
                                 :test #'equal)))
            (if section
                (progn 
                  (setf *config* (apply #'make-instance 'blog (cdr section)))
                  (setf (slot-value *config* 'repo) config-key))
                (error 'unknown-config-section-error 
                       :text (format nil "In ~A: No such key: '~A'." in config-key)))))
      (load-plugins (plugins *config*)))))
