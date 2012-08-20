(in-package :coleslaw)

(defparameter *posts* (make-hash-table :test #'equal))

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
        (setf (gethash (post-slug post) *posts*) post))))
  (maphash #'write-post *posts*))

(defun read-post (stream)
  "Make a POST instance based on the data from STREAM."
  )

(defun write-post (slug post)
  "Write out the HTML for POST in SLUG.html."
  (render-page (format nil "posts/~a.html" slug)
               (funcall (theme-fn "POST")
                        (list :title (post-title post)
                              :tags (post-tags post)
                              :date (post-date post)
                              :content (post-content post)
                              ; TODO: Populate prev and next with links.
                              :prev nil
                              :next nil))))

(defun slug-char-p (char)
  (or (char<= #\0 char #\9)
      (char<= #\a char #\z)
      (char<= #\A char #\Z)
      (member char '(#\_ #\-))))

(defun slugify (string)
  (remove-if-not #'slug-char-p (substitute #\- #\Space string)))
