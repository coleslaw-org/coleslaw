(in-package :coleslaw)

(defparameter *posts* (make-hash-table :test #'equal)
  "A hash table to store all the posts and their metadata.")

(defclass post ()
  ((slug :initform nil :initarg :slug :accessor post-slug)
   (title :initform nil :initarg :title :accessor post-title)
   (tags :initform nil :initarg :tags :accessor post-tags)
   (date :initform nil :initarg :date :accessor post-date)
   (format :initform nil :initarg :format :accessor post-format)
   (content :initform nil :initarg :content :accessor post-content)))

(defun render-posts ()
  "Iterate through the files in the repo to render+write the posts out to disk."
  (clrhash *posts*)
  (do-files (file (repo *config*) "post")
    (with-open-file (in file)
      (let ((post (read-post in)))
        (if (gethash (post-slug post) *posts*)
            (error "There is already an existing post with the slug ~a."
                   (post-slug post))
            (setf (gethash (post-slug post) *posts*) post)))))
  (maphash #'write-post *posts*))

(defgeneric render-content (text format)
  (:documentation "Compile TEXT from the given FORMAT to HTML for display.")
  (:method (text (format (eql :html)))
    text))

(defun read-post (in)
  "Make a POST instance based on the data from the stream IN."
  (flet ((check-header ()
           (unless (string= (read-line in) ";;;;;")
             (error "The provided file lacks the expected header.")))
         (parse-field (str)
           (nth-value 1 (cl-ppcre:scan-to-strings "[a-zA-Z]+: (.*)" str)))
         (slurp-remainder ()
           (read-sequence (make-string (- (file-length in)
                                          (file-position in))
                                       :element-type 'character) in)))
    (check-header)
    (let ((args (loop for field in '("title" "tags" "date" "format")
                   for line = (read-line in nil)
                   appending (list (make-keyword field)
                                   (aref (parse-field (read-line in)) 0)))))
    (check-header)
    (setf (getf args :tags) (cl-ppcre:split ", " (getf args :tags))
          (getf args :format) (make-keyword (getf args :format)))
    (apply 'make-instance 'blog
           (append args (list :content (slurp-remainder)
                              :slug (slugify (getf args :title))))))))

(defun write-post (slug post)
  "Write out the HTML for POST in SLUG.html."
  (render-page (format nil "posts/~a.html" slug)
               (funcall (theme-fn "POST")
                        (list :title (post-title post)
                              :tags (post-tags post)
                              :date (post-date post)
                              :content (render-content (post-content post)
                                                       (post-format post))
                              ; TODO: Populate prev and next with links.
                              :prev nil
                              :next nil))))

(defun slug-char-p (char)
  "Determine if CHAR is a valid slug (i.e. URL) character."
  (or (char<= #\0 char #\9)
      (char<= #\a char #\z)
      (char<= #\A char #\Z)
      (member char '(#\_ #\- #\.))))

(defun slugify (string)
  "Return a version of STRING suitable for use as a URL."
  (remove-if-not #'slug-char-p (substitute #\- #\Space string)))
