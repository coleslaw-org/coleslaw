(defpackage :coleslaw-hard-versioned
  (:use :cl)
  (:import-from :coleslaw
                #:*config*
                #:run-program)
  (:import-from :uiop #:ensure-directory-pathname)
  (:export #:hard-versioned))
(in-package :coleslaw-hard-versioned)

(defun hard-versioned (staging) (declare (ignore staging)))
(defun enable (backup-dir)
  (setf (symbol-function 'hard-versioned)
        (lambda (staging)
          (run-program "tar -cjf \"~Asite-~S.tar.bz2\" \"~A\""
                       (ensure-directory-pathname backup-dir)
                       (get-universal-time)
                       staging))))
(defmethod coleslaw:deploy (staging)
  (hard-versioned staging))

