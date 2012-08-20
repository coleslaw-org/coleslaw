(in-package :coleslaw)

(defparameter *metadata* (make-hash-table :test #'equal))

(defclass post ()
  ((slug :initform nil :initarg :slug :accessor post-slug)
   (title :initform nil :initarg :title :accessor post-title)
   (tags :initform nil :initarg :tags :accessor post-tags)
   (date :initform nil :initarg :date :accessor post-date)
   (format :initform nil :initarg :format :accessor post-format)
   (content :initform nil :initarg :content :accessor post-content)
   (aliases :initform nil :initarg :aliases :accessor post-aliases)))

(defun render-posts ()
  (do-files (file (repo *config*) "post")
    (with-open-file (in file)
      (let ((post (read-post in)))
        (setf (gethash (post-slug post) *metadata*) post))))
  (maphash #'write-post *metadata*))

(defun read-post (stream)
  "Make a POST instance based on the data from STREAM."

  )

(defun write-post (slug post)
  "Write out the HTML for POST in SLUG.html."
  (with-open-file (out (format nil "~a.html" slug)
                       :direction :output
                       :if-does-not-exist :create)
    (let ((content (funcall (theme-fn "POST")
                            (list :title (post-title post)
                                  :tags (post-tags post)
                                  :date (post-date post)
                                  :content (post-content post)
                                  :prev nil
                                  :next nil))))
      (write content out))))
