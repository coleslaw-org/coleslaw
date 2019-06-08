(defpackage :coleslaw-versioned
  (:use :cl)
  (:import-from :coleslaw #:*config*
                #:deploy-dir
                #:rsync-ensure-directories-exist
                #:staging-dir
                #:rel-path
                #:run-program
                #:update-symlink
                #:path-remove
                #:path-move)
  (:import-from :uiop #:subpathname))

(in-package :coleslaw-versioned)

(defparameter *symlink-dir* "generated/~a")
(defun local (staging)
  (let* ((new-build (rel-path staging *symlink-dir* (get-universal-time)))
         (prev (rel-path staging ".prev"))
         (curr (rel-path staging ".curr")))
    (ensure-directories-exist new-build)
    (when (and (probe-file prev) (truename prev))
      (path-remove (truename prev)))
    (when (probe-file curr)
      (update-symlink prev (truename curr)))
    (update-symlink curr new-build)))

(defun deploy (staging)
  (let ((dest (rel-path (deploy-dir *config*)
                        *symlink-dir*
                        (get-universal-time))))
    (rsync-ensure-directories-exist dest)
    (path-move staging dest)))

(defmethod coleslaw:deploy :before (staging)
  (local staging)
  (deploy staging))

(defun enable ())
