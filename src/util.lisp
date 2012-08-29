(in-package :coleslaw)

(defun app-path (path &rest args)
  "Take a relative PATH and return the corresponding pathname beneath coleslaw.
If ARGS is provided, use (apply 'format nil PATH ARGS) as the value of PATH."
  (merge-pathnames (apply 'format nil path args) coleslaw-conf:*basedir*))

(defun run-program (program &rest args)
  "Take a PROGRAM and execute the corresponding shell command. If ARGS is provided,
use (apply 'format nil PROGRAM ARGS) as the value of PROGRAM."
  (trivial-shell:shell-command (apply 'format nil program args)))

(defun update-symlink (path target)
  "Update the symlink at PATH to point to TARGET."
  (run-program "ln -sfn ~a ~a" target path))

(defmacro do-files ((var path &optional extension) &body body)
  "For each file on PATH, run BODY. If EXTENSION is provided, only run BODY
on files that match the given extension."
  (alexandria:with-gensyms (ext)
    `(dolist (,var (cl-fad:list-directory ,path))
       ,@(if extension
             `((let ((,ext (pathname-type ,var)))
                 (when (and ,ext (string= ,ext ,extension))
                   ,@body)))
             `,body))))
