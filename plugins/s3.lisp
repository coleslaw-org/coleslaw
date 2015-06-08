(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'zs3))

(defpackage :coleslaw-s3
  (:use :cl)
  (:import-from :coleslaw #:deploy
                          #:deploy-dir
                          #:*config*)
  (:export #:enable))

(in-package :coleslaw-s3)

(defparameter *content-type-map* '(("html" . "text/html")
                                   ("css" . "text/css")
                                   ("png" . "image/png")
                                   ("jpg" . "image/jpg"))
  "A mapping from file extensions to content types.")

(defparameter *cache* (make-hash-table :test #'equal)
  "A cache of keys in a given bucket hashed by etag.")

(defparameter *bucket* nil
  "A string designating the bucket to upload to.")

(defun content-type (extension)
  (cdr (assoc extension *content-type-map* :test #'equal)))

(defun stale-keys ()
  (loop for key being the hash-values in *cache* collecting key))

(defun s3-sync (filepath dir)
  (let ((etag (zs3:file-etag filepath))
        (key (enough-namestring filepath dir)))
    (if (gethash etag *cache*)
        (remhash etag *cache*)
        (zs3:put-file filepath *bucket* key :public t
                      :content-type (content-type (pathname-type filepath))))))

(defun dir->s3 (dir)
  (flet ((upload (file) (s3-sync file dir)))
    (cl-fad:walk-directory dir #'upload)))

(defmethod deploy :after (staging)
  (let ((blog (deploy-dir *config*)))
    (loop for key across (zs3:all-keys *bucket*)
       do (setf (gethash (zs3:etag key) *cache*) key))
    (dir->s3 blog)
    (zs3:delete-objects (stale-keys) *bucket*)))

(defun enable (&key auth-file bucket)
  "AUTH-FILE: Path to file with the access key on the first line and the secret 
   key on the second."
  (setf zs3:*credentials* (zs3:file-credentials auth-file)
        *bucket* bucket))
