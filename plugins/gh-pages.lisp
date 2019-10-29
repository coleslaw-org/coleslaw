(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'puri :silent t))
(defpackage :coleslaw-gh-pages
  (:use :cl)
  (:import-from :coleslaw
                #:*config*
                #:domain
                #:deploy
                #:staging-dir
                #:deploy-dir)
  (:export #:enable))

(in-package :coleslaw-gh-pages)

(defvar *options* nil)

(defmethod deploy (staging)
  (uiop:run-program (list* (namestring
                            (merge-pathnames "plugins/publish-gh-pages.sh"
                                             coleslaw-conf:*basedir*))
                           (namestring
                            (merge-pathnames (staging-dir *config*)))
                           (namestring
                            (merge-pathnames (deploy-dir *config*)))
                           *options*)
                    :output t
                    :error-output t))

(defun enable (&key url (branch "gh-pages") (remote "origin") cname)
  (check-type url string)
  (check-type remote string)
  (check-type branch string)
  (if (eq t cname)
      (progn
        (setf cname (puri:uri-host (puri:parse-uri (domain *config*))))
        (check-type cname string)
        (setf *options* (list url branch remote cname)))
      (setf *options* (list url branch remote))))
