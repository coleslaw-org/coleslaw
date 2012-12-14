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

(defmethod render ((object post) &key prev next)
  (funcall (theme-fn 'post) (list :config *config*
                                  :post object
                                  :prev prev
                                  :next next)))

(defmethod page-path ((object post))
  (rel-path (staging *config*) "posts/~a" (post-slug object)))

(defun read-post (in)
  "Make a POST instance based on the data from the stream IN."
  (flet ((check-header ()
           (unless (string= (read-line in) ";;;;;")
             (error "The provided file lacks the expected header.")))
         (parse-field (str)
           (nth-value 1 (cl-ppcre:scan-to-strings "[a-zA-Z]+: (.*)" str)))
         (field-name (line)
           (subseq line 0 (position #\: line)))
         (read-tags (str)
           (mapcar #'string-downcase (cl-ppcre:split ", " str)))
         (slurp-remainder ()
           (let ((seq (make-string (- (file-length in) (file-position in)))))
             (read-sequence seq in)
             (remove #\Nul seq))))
    (check-header)
    (let ((args (loop for line = (read-line in nil) until (string= line ";;;;;")
                   appending (list (make-keyword (string-upcase (field-name line)))
                                   (aref (parse-field line) 0)))))
      (setf (getf args :tags) (read-tags (getf args :tags))
            (getf args :format) (make-keyword (string-upcase (getf args :format))))
      (apply 'make-instance 'post
             (append args (list :content (render-content (slurp-remainder)
                                                         (getf args :format))
                                :slug (slugify (getf args :title))))))))

(defun load-posts ()
  "Read the stored .post files from the repo."
  (clrhash *posts*)
  (do-files (file (repo *config*) "post")
    (with-open-file (in file)
      (let ((post (read-post in)))
        (if (gethash (post-slug post) *posts*)
            (error "There is already an existing post with the slug ~a."
                   (post-slug post))
            (setf (gethash (post-slug post) *posts*) post))))))

(defun render-posts ()
  "Iterate through the files in the repo to render+write the posts out to disk."
  (loop for (prev post next) on (append '(nil) (sort (hash-table-values *posts*)
                                                     #'string< :key #'post-date))
     while post do (write-page (page-path post)
                               (render-page post nil :prev prev :next next))))

(defun slug-char-p (char)
  "Determine if CHAR is a valid slug (i.e. URL) character."
  (or (char<= #\0 char #\9)
      (char<= #\a char #\z)
      (char<= #\A char #\Z)
      (member char '(#\_ #\- #\.))))

(defun slugify (string)
  "Return a version of STRING suitable for use as a URL."
  (remove-if-not #'slug-char-p (substitute #\- #\Space string)))
