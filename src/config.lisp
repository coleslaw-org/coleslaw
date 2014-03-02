(in-package :coleslaw)

(defclass blog ()
  ((author          :initarg :author         :accessor author)
   (deploy-dir      :initarg :deploy-dir     :accessor deploy-dir)
   (domain          :initarg :domain         :accessor domain)
   (feeds           :initarg :feeds          :accessor feeds)
   (license         :initarg :license        :accessor license)
   (plugins         :initarg :plugins        :accessor plugins)
   (repo            :initarg :repo           :accessor repo)
   (sitenav         :initarg :sitenav        :accessor sitenav)
   (staging-dir     :initarg :staging-dir    :accessor staging-dir)
   (postsdir        :initarg :postsdir       :accessor postsdir      :initform "posts") 
   (separator       :initarg :separator      :accessor separator     :initform ";;;;;")
   (pageext         :initarg :pageext        :accessor pageext       :initform ".html")
   (title           :initarg :title          :accessor title)
   (theme           :initarg :theme          :accessor theme)))

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
  (setf *injections* nil)
  (flet ((plugin-path (sym)
           (app-path "plugins/~a" (string-downcase (symbol-name sym)))))
    (dolist (plugin plugins)
      (destructuring-bind (name &rest args) plugin
        (apply 'enable-plugin (plugin-path name) args)))))

(defun discover-config-path (&optional (path ""))
  (let ((default-path (make-pathname :directory (namestring (user-homedir-pathname)) :name ".coleslawrc"))
        (custom-path (make-pathname :directory path :name ".coleslawrc")))
    (cond
      ((file-exists-p custom-path) custom-path)
      ((file-exists-p default-path) default-path))))

(defun load-config (config-key)
  "Load the coleslaw configuration from DIR/.coleslawrc, using CONFIG-KEY
if necessary. DIR is ~ by default."

  (with-open-file (in (discover-config-path config-key))
    (let ((config-form (read in)))
      (if (symbolp (car config-form))
          ;; Single site config: ignore CONFIG-KEY.
          (setf *config* (apply #'make-instance 'blog config-form))
          ;; Multi-site config: load config section for CONFIG-KEY.
          (let* ((config-key-pathname (cl-fad:pathname-as-directory config-key))
                 (section (assoc config-key-pathname config-form
                                 :key #'cl-fad:pathname-as-directory
                                 :test #'equal)))
            (if section
                (setf *config* (apply #'make-instance 'blog (cdr section))
                      (repo *config*) config-key)
                (error 'unknown-config-section-error
                       :text (format nil "In ~A: No such key: '~A'." in config-key)))))
      (load-plugins (plugins *config*)))))
