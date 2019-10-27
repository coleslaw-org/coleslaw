
(defpackage :coleslaw-rsync
  (:use :cl)
  (:import-from :coleslaw #:*config*
                          #:deploy
                          #:deploy-dir)
  (:export #:enable))

(in-package :coleslaw-rsync)

(defvar *args* nil)

(defmethod deploy (staging)
  (coleslaw::run-program "rsync ~{~A~^ ~} ~A ~A" *args*
                         (merge-pathnames staging)
                         (merge-pathnames (deploy-dir *config*))))

(defun enable (&rest args)
  (setf *args* args))
