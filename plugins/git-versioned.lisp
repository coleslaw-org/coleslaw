(defpackage :coleslaw-git-versioned
  (:use :cl)
  (:import-from :coleslaw
                #:*config*
                #:run-lines)
  (:import-from :uiop #:ensure-directory-pathname))

(in-package :coleslaw-git-versioned)

(defconstant +nothing-to-commit+ 1
  "Error code when git-commit has nothing staged to commit.")

;; These have their symbol-functions set in order to close over the src-dir
;; variable.
(defun git-versioned ()
  "Run all git commands as specified in the .coleslawrc.")
(defun command (args)
  "Automatically git commit and push the blog to remote."
  (declare (ignore args)))

(defun enable (src-dir &rest commands)
  "Define git-versioned functions at runtime."
  (setf (symbol-function 'git-versioned)
        (lambda ()
          (loop for fsym in commands
                do (funcall (symbol-function (intern (symbol-name fsym)
                                                     :coleslaw-git-versioned))))))
  (setf (symbol-function 'command)
        (lambda (args)
          (run-lines src-dir
                     (format nil "git ~A" args)))))
(defmethod coleslaw:deploy :before (staging)
  (declare (ignore staging))
  (git-versioned))

(defun stage () (command "stage -A"))
(defun commit (&optional (commit-message "Automatic commit."))
  (handler-case (command (format nil "commit -m '~A'" commit-message))
    (uiop/run-program:subprocess-error (error)
      (case (uiop/run-program:subprocess-error-code error)
        (+nothing-to-commit+ (format t "Nothing to commit. Error ~d"
                                     +nothing-to-commit+))
        (otherwise (error error))))))

(defun upload ()
  (command "push"))
