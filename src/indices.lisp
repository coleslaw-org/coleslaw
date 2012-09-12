(in-package :coleslaw)

(defclass index ()
  ((path :initform nil :initarg :path :accessor index-path)
   (posts :initform nil :initarg :posts :accessor index-posts)
   (title :initform nil :initarg :title :accessor index-title)))

(defmethod render ((content index) &key prev next)
  (funcall (theme-fn 'index) (list :tags (all-tags)
                                   :months (all-months)
                                   :config *config*
                                   :index content
                                   :prev prev
                                   :next next)))

(defun all-months ()
  "Retrieve a list of all months with published posts."
  (sort (remove-duplicates (mapcar (lambda (x) (get-month (post-date x)))
                                   (hash-table-values *posts*)) :test #'string=)
        #'string>))

(defun all-tags ()
  "Retrieve a list of all tags used in posts."
  (sort (remove-duplicates (mappend 'post-tags (hash-table-values *posts*))
                           :test #'string=) #'string<))

(defun get-month (timestamp)
  "Extract the YYYY-MM portion of TIMESTAMP."
  (subseq timestamp 0 7))

(defun by-date (posts)
  "Sort POSTS in reverse chronological order."
  (sort posts #'string> :key #'post-date))

(defun index-by-tag (tag posts)
  "Return an index of all POSTS matching the given TAG."
  (let ((content (remove-if-not (lambda (post) (member tag (post-tags post)
                                                       :test #'string=)) posts)))
    (make-instance 'index :path (format nil "tag/~a.html" tag)
                          :posts content
                          :title (format nil "Posts tagged ~a" tag))))

(defun index-by-month (month posts)
  "Return an index of all POSTS matching the given MONTH."
  (let ((content (remove-if-not (lambda (post) (search month (post-date post)))
                                posts)))
    (make-instance 'index :path (format nil "date/~a.html" month)
                          :posts content
                          :title (format nil "Posts from ~a" month))))

(defun index-by-n (i posts &optional (step 10))
  "Return the index for the Ith page of POSTS in reverse chronological order."
  (make-instance 'index :path (format nil "~d.html" (1+ i))
                        :posts (let ((index (* step i)))
                                 (subseq posts index (min (length posts)
                                                          (+ index step))))
                        :title "Recent Posts"))

(defun render-indices ()
  "Render the indices to view posts in groups of size N, by month, and by tag."
  (let ((posts (by-date (hash-table-values *posts*))))
    (dolist (tag (all-tags))
      (render-page (index-by-tag tag posts)))
    (dolist (month (all-months))
      (render-page (index-by-month month posts)))
    (dotimes (i (ceiling (length posts) 10))
      (render-page (index-by-n i posts) nil
                   :prev (and (plusp i) i)
                   :next (and (< (* i 10) (length posts)) (+ 2 i)))))
  (update-symlink "index.html" "1.html"))
