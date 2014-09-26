(defpackage :coleslaw-versioned
  (:use :cl)
  (:import-from :coleslaw #:*config*
                          #:deploy-dir
                          #:rel-path
                          #:run-program
                          #:update-symlink))

(in-package :coleslaw-versioned)

(defmethod coleslaw:deploy (staging)
  (let* ((dest (deploy-dir *config*))
         (new-build (rel-path dest "generated/~a" (get-universal-time)))
         (prev (rel-path dest ".prev"))
         (curr (rel-path dest ".curr")))
    (ensure-directories-exist new-build)
    (run-program "mv ~a ~a" staging new-build)
    (when (and (probe-file prev) (truename prev))
      (run-program "rm -r ~a" (truename prev)))
    (when (probe-file curr)
      (update-symlink prev (truename curr)))
    (update-symlink curr new-build)))

(defun enable ())
