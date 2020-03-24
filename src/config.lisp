(in-package :coleslaw)

(defclass blog ()
  ((author          :initarg :author         :reader author)
   (charset         :initarg :charset        :reader charset)
   (deploy-dir      :initarg :deploy-dir     :reader deploy-dir)
   (domain          :initarg :domain         :reader domain)
   (excerpt-sep     :initarg :excerpt-sep    :reader excerpt-sep)
   (feeds           :initarg :feeds          :reader feeds)
   (name-fn         :initarg :name-fn        :reader name-fn)
   (lang            :initarg :lang           :reader lang)
   (license         :initarg :license        :reader license)
   (page-ext        :initarg :page-ext       :reader page-ext-intolerant)
   (plugins         :initarg :plugins        :reader plugins)
   (repo            :initarg :repo           :accessor repo-dir)
   (routing         :initarg :routing        :reader routing)
   (separator       :initarg :separator      :reader separator)
   (sitenav         :initarg :sitenav        :reader sitenav)
   (staging-dir     :initarg :staging-dir    :reader staging-dir)
   (theme           :initarg :theme          :reader theme)
   (title           :initarg :title          :reader title)
   (index-ext       :initarg :index-ext      :reader index-ext))
  (:default-initargs
   :feeds        nil
   :license      nil
   :plugins      '((rsync "-avz" "--delete" "--exclude" ".git/" "--exclude" ".gitignore" "--copy-links"))
   :sitenav      nil
   :excerpt-sep  "<!--more-->"
   :name-fn      'identity
   :charset      "UTF-8"
   :lang         "en"
   :page-ext     #1="html"
   :separator    ";;;;;"
   :staging-dir  "/tmp/coleslaw"
   :index-ext    #1#))

(defun page-ext (config)
  "Get page extension, and allow for an extensionless system."
  (let ((ext (page-ext-intolerant config)))
    (if (string= ext "")
        ""
        (concatenate 'string "." ext))))
(defun dir-slot-reader (config name)
  "Take CONFIG and NAME, and return a directory pathname for the matching SLOT."
  (ensure-directory-pathname (slot-value config name)))

(defmethod deploy-dir  ((config blog)) (dir-slot-reader config 'deploy-dir))
(defmethod repo-dir    ((config blog)) (dir-slot-reader config 'repo))
(defmethod staging-dir ((config blog)) (dir-slot-reader config 'staging-dir))

(defparameter *config* nil
  "A variable to store the blog configuration and plugin settings.")

(define-condition plugin-conf-error ()
  ((plugin  :initarg :plugin :reader plugin)
   (message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream "~A: ~A" (plugin condition) (message condition))))
  (:documentation "Condition to signal when the plugin is misconfigured."))

(defun enable-plugin (name args)
  "Given a plugin, NAME, compile+load it and call its ENABLE function with ARGS."
  (flet ((plugin-path (sym)
           (if (probe-file (repo-path "plugins/~(~A~).lisp" sym))
               (repo-path "plugins/~(~A~)" sym)
               (app-path "plugins/~(~A~)" sym)))
         (plugin-package (sym)
           (format nil "~:@(coleslaw-~A~)" sym)))
    (let ((file (plugin-path name)))
      (multiple-value-bind (output-file error)
          (ignore-errors (compile-file file :verbose nil :print nil))
        (when error
          (warn "Error while compiling plugin ~A: ~A.~%" name error))
        (load (or output-file file) :verbose t)))
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

(defun load-config (&optional (repo-dir ""))
  "Find and load the coleslaw configuration from .coleslawrc. REPO-DIR will be
preferred over the home directory if provided."
  (with-open-file (in (discover-config-path repo-dir) :external-format :utf-8)
    (let ((config-form (read in)))
      (setf *config* (construct 'blog config-form)
            (repo-dir *config*) repo-dir)))
  (load-plugins (plugins *config*)))
