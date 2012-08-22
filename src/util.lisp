(in-package :coleslaw)

(defun app-path (path &rest args)
  "Take a relative PATH and return the corresponding pathname beneath coleslaw.
If ARGS is provided, use (apply 'format nil PATH ARGS) as the value of PATH."
  (merge-pathnames (apply 'format nil path args) coleslaw-conf:*basedir*))

(defun to-pathname (file &optional (parent coleslaw-conf:*basedir*))
  "Convert an iolib file-path back to a pathname."
  (merge-pathnames (file-path-namestring file) parent))

(defun read-symlink (path)
  "A trivial wrapper over iolib.os that returns the pathname in the symlink PATH."
  (to-pathname (iolib.os:read-symlink path)))

(defmacro do-files ((var path &optional extension) &body body)
  "For each file under PATH, run BODY. If EXTENSION is provided, only run BODY
on files that match the given extension."
  (alexandria:with-gensyms (ext)
    `(mapcar (lambda (,var)
               ,@(if extension
                     `((let ((,ext (pathname-type ,var)))
                         (when (and ,ext (string= ,ext ,extension))
                           ,@body)))
                     `,body))
             (list-directory ,path))))
