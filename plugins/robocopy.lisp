
(defpackage :coleslaw-robocopy
  (:use :cl)
  (:import-from :coleslaw #:*config*
                #:deploy
                #:deploy-dir)
  (:export #:enable))

(in-package :coleslaw-robocopy)

(defvar *args* nil)

(defmethod deploy (staging)
  (coleslaw::run-program
   "(robocopy ~A ~A ~{~A~^ ~}) ^& IF %ERRORLEVEL% LEQ 1 exit 0"
   (merge-pathnames staging)
   (merge-pathnames (deploy-dir *config*))
   *args*))

(defun enable (&rest args)
  (setf *args* args))
