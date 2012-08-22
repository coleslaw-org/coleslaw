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

(defun post-url (post)
  "Return the relative URL for a given post."
  (format nil "posts/~a.html" (post-slug post)))

(defun render-posts ()
  "Iterate through the files in the repo to render+write the posts out to disk."
  (load-posts)
  (loop with posts = (sort (hash-table-values *posts*) #'string< :key #'post-date)
     for i from 1 upto (length posts)
     for prev = nil then post
     for post = (nth (1- i) posts)
     for next = (nth (1+ i) posts)
     do (write-post post :prev (and prev (post-url prev))
                         :next (and next (post-url next)))))

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
           (nth-value 1 (cl-ppcre:scan-to-strings "[a-zA-Z]+: (.*)" str))))
    (check-header)
    (let ((args (loop for field in '("title" "tags" "date" "format")
                   for line = (read-line in nil)
                   appending (list (make-keyword (string-upcase field))
                                   (aref (parse-field line) 0)))))
    (check-header)
    (setf (getf args :tags) (cl-ppcre:split ", " (getf args :tags))
          (getf args :format) (make-keyword (string-upcase (getf args :format))))
    (apply 'make-instance 'post
           (append args (list :content (read-line in nil)
                              :slug (slugify (getf args :title))))))))

(defun write-post (post &key prev next)
  "Write out the HTML for POST in SLUG.html."
  (render-page (post-url post)
               (funcall (theme-fn "POST")
                        (list :title (post-title post)
                              :tags (post-tags post)
                              :date (post-date post)
                              :content (render-content (post-content post)
                                                       (post-format post))
                              :prev prev
                              :next next))))

(defun slug-char-p (char)
  "Determine if CHAR is a valid slug (i.e. URL) character."
  (or (char<= #\0 char #\9)
      (char<= #\a char #\z)
      (char<= #\A char #\Z)
      (member char '(#\_ #\- #\.))))

(defun slugify (string)
  "Return a version of STRING suitable for use as a URL."
  (remove-if-not #'slug-char-p (substitute #\- #\Space string)))
