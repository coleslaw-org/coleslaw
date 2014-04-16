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

(defun discover-config-path (path)
  "Check the supplied PATH for a .coleslawrc and if one
doesn't exist, use the .coleslawrc in the home directory."
  (let ((custom-path (rel-path path ".coleslawrc")))
    (if (file-exists-p custom-path)
        custom-path
        (rel-path (user-homedir-pathname) ".coleslawrc"))))

(defun load-config (&optional (config-key ""))
  "Load the coleslaw configuration from DIR/.coleslawrc, using CONFIG-KEY
if necessary. DIR is ~ by default."
  (with-open-file (in (discover-config-path config-key) :external-format '(:utf-8))
    (let ((config-form (read in)))
      (if (symbolp (car config-form))
          ;; Single site config: ignore CONFIG-KEY.
          (setf *config* (construct 'blog config-form))
          ;; Multi-site config: load config section for CONFIG-KEY.
          (let* ((config-key-pathname (cl-fad:pathname-as-directory config-key))
                 (section (assoc config-key-pathname config-form
                                 :key #'cl-fad:pathname-as-directory
                                 :test #'equal)))
            (if section
                (setf *config* (construct 'blog (cdr section))
                      (repo *config*) config-key)
                (error 'unknown-config-section-error
                       :text (format nil "In ~A: No such key: '~A'." in config-key)))))
      (load-plugins (plugins *config*)))))
