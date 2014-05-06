(in-package :coleslaw)

(defclass blog ()
  ((author          :initarg :author         :accessor author)
   (deploy-dir      :initarg :deploy-dir     :accessor deploy-dir)
   (domain          :initarg :domain         :accessor domain)
   (feeds           :initarg :feeds          :accessor feeds)
   (license         :initarg :license        :accessor license)
   (page-ext        :initarg :page-ext       :accessor page-ext       :initform "html")
   (plugins         :initarg :plugins        :accessor plugins)
   (repo            :initarg :repo           :accessor repo)
   (routing         :initarg :routing        :accessor routing)
   (separator       :initarg :separator      :accessor separator      :initform ";;;;;")
   (sitenav         :initarg :sitenav        :accessor sitenav)
   (staging-dir     :initarg :staging-dir    :accessor staging-dir    :initform "/tmp/coleslaw/")
   (theme           :initarg :theme          :accessor theme)
   (title           :initarg :title          :accessor title)))

(defparameter *config* nil
  "A variable to store the blog configuration and plugin settings.")

(defun enable-plugin (name args)
  "Given a plugin, NAME, compile+load it and call its ENABLE function with ARGS."
  (flet ((plugin-path (sym)
           (app-path "plugins/~(~A~)" sym))
         (plugin-package (sym)
           (format nil "~:@(coleslaw-~A~)" sym)))
    (let ((file (plugin-path name)))
      (load (compile-file file :verbose nil :print nil) :verbose t))
    (let ((package (find-package (plugin-package name))))
      (apply (find-symbol "ENABLE" package) args))))

(defun load-plugins (plugins)
  "Compile and load the listed PLUGINS. It is expected that matching *.lisp files
are in the plugins folder in coleslaw's source directory."
  (setf *injections* nil)
  (dolist (plugin plugins)
    (destructuring-bind (name &rest args) plugin
      (enable-plugin name args))))

(defun discover-config-path (repo-path)
  "Check the supplied REPO-PATH for a .coleslawrc and if one
doesn't exist, use the .coleslawrc in the home directory."
  (let ((repo-config (rel-path repo-path ".coleslawrc")))
    (if (file-exists-p repo-config)
        repo-config
        (rel-path (user-homedir-pathname) ".coleslawrc"))))

(defun load-config (&optional repo-dir)
  "Find and load the coleslaw configuration from .coleslawrc. REPO-DIR will be
preferred over the home directory if provided."
  (with-open-file (in (discover-config-path repo-dir) :external-format '(:utf-8))
    (let ((config-form (read in)))
      (setf *config* (construct 'blog config-form))))
  (load-plugins (plugins *config*)))
