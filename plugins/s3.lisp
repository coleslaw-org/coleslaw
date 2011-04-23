(eval-when (:compile-toplevel)
  (ql:quickload '(zs3)))

(defpackage :coleslaw-s3
  (:use :cl :coleslaw :zs3))

(in-package :coleslaw-s3)

(defparameter *credentials* (get-credentials :s3)
  "The credentials to authenticate with Amazon Web Services.
Stored in a file with the access key on the first line
and the secret key on the second.")

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

(defun init ()
  (unless *credentials*
    (set-credentials :s3 (file-credentials "~/.aws"))
    (setf *credentials* (get-credentials :s3))))

(defun stale-keys (&key cache)
  (loop for key being the hash-values in cache collecting key))

(defun s3-sync (filepath &key bucket dir public-p cache)
  (flet ((compute-key (namestring)
           (subseq namestring (length (namestring (truename dir))))))
    (let* ((etag (file-etag filepath))
           (namestring (namestring filepath))
           (key (compute-key namestring)))
      (if (gethash etag cache)
          (remhash etag cache)
          (put-file filepath bucket key :public public-p
                    :content-type (content-type (pathname-type filepath)))))))

(defun dir->s3 (dir &key bucket cache public-p)
  (cl-fad:walk-directory dir (lambda (file)
                               (s3-sync file :cache cache :dir dir
                                        :bucket bucket :public-p public-p))))

(defmethod coleslaw::render-site :after ()
  (init)
  (let* ((keys (all-keys *bucket*)))
    (loop for key across keys do (setf (gethash (etag key) *cache*) key))
    (dir->s3 coleslaw::*output-dir* :bucket *bucket* :cache *cache* :public-p t)
    (when (stale-keys :cache *cache*)
      (delete-objects (stale-keys) *bucket*))))
