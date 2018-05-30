(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'puri))

(defpackage :coleslaw-gh-pages
  (:use :cl)
  (:import-from :puri #:parse-uri #:uri-host)
  (:import-from :coleslaw #:*config*
                          #:deploy
                          #:deploy-dir
                          #:domain
                          #:rel-path)
  (:export #:enable))

(in-package :coleslaw-gh-pages)

(defvar *cname* nil
  "The domain CNAME for github to serve pages from.")

(defmethod deploy :after (staging)
  (let ((blog (rel-path (deploy-dir *config*) ".curr")))
    (delete-file (rel-path blog "index.html"))
    (cl-fad:copy-file (rel-path blog "1.html") (rel-path blog "index.html"))
    (with-open-file (out (rel-path blog "CNAME")
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (format out "~A~%" *cname*))))

(defun enable (&key cname)
  (typecase cname
    (string (setf *cname* cname))
    (t (setf *cname* (uri-host (parse-uri (domain *config*)))))
    (otherwise (error "Not a valid CNAME: ~A" cname))))
