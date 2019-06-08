(defpackage :coleslaw-git-versioned
  (:use :cl)
  (:import-from :coleslaw
                #:*config*
                #:run-lines)
  (:import-from :uiop #:ensure-directory-pathname))
(in-package :coleslaw-git-versioned)

(defun git-versioned ())
(defun enable (&rest commands)
  (setf (symbol-function 'git-versioned)
        (lambda ()
          "Run all git commands as specified in the .coleslawrc."
          (mapc #'funcall
                (loop for fsym in commands
                      collect (symbol-function (intern (symbol-name fsym)
                                                       :coleslaw-git-versioned)))))))

(defun command (args) (declare (ignore args)))
(defmethod coleslaw:deploy :before (staging)
  (setf (symbol-function 'command)
        (lambda (args)
          "Automatically git commit and push the blog to remote."
          (run-lines staging
                     (format nil "git ~A" args))))
  (git-versioned))

(defun stage ()
  (command "stage -A"))
(defun commit (&optional (commit-message "Automatic commit."))
  (command (format nil "commit -m '~A'" commit-message)))
(defun upload ()
  (command "push"))
