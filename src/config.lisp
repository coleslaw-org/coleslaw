(in-package :coleslaw)

(defparameter *config* nil
  "A variable to store the blog configuration and plugin settings.")

(defun load-config (&optional (dir (user-homedir-pathname)))
  "Load the coleslaw configuration from DIR/.coleslawrc. DIR is ~ by default."
  (with-open-file (in (merge-pathnames ".coleslawrc" dir))
    (setf *config* (apply #'make-instance 'blog (read in)))))
