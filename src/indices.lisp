(in-package :coleslaw)

(defclass index ()
  ((id :initform nil :initarg :id :accessor index-id)
   (posts :initform nil :initarg :posts :accessor index-posts)
   (title :initform nil :initarg :title :accessor index-title)))

(defmethod render ((object index) &key prev next)
  (funcall (theme-fn 'index) (list :tags (all-tags)
                                   :months (all-months)
                                   :config *config*
                                   :index object
                                   :prev prev
                                   :next next)))

(defclass tag-index (index) ())
(defclass date-index (index) ())
(defclass int-index (index) ())

(defmethod page-path ((object index))
  (rel-path (staging *config*) (index-id object)))
(defmethod page-path ((object tag-index))
  (rel-path (staging *config*) "tag/~a" (index-id object)))
(defmethod page-path ((object date-index))
  (rel-path (staging *config*) "date/~a" (index-id object)))
(defmethod page-path ((object int-index))
  (rel-path (staging *config*) "~d" (index-id object)))

(defun all-months ()
  "Retrieve a list of all months with published posts."
  (sort (remove-duplicates (mapcar (lambda (x) (get-month (content-date x)))
                                   (hash-table-values *content*)) :test #'string=)
        #'string>))

(defun all-tags ()
  "Retrieve a list of all tags used in posts."
  (sort (remove-duplicates (mappend 'content-tags (hash-table-values *content*))
                           :test #'string=) #'string<))

(defun get-month (timestamp)
  "Extract the YYYY-MM portion of TIMESTAMP."
  (subseq timestamp 0 7))

(defun index-by-tag (tag posts)
  "Return an index of all POSTS matching the given TAG."
  (let ((content (remove-if-not (lambda (post) (member tag (content-tags post)
                                                       :test #'string=)) posts)))
    (make-instance 'tag-index :id tag
                              :posts content
                              :title (format nil "Posts tagged ~a" tag))))

(defun index-by-month (month posts)
  "Return an index of all POSTS matching the given MONTH."
  (let ((content (remove-if-not (lambda (post) (search month (content-date post)))
                                posts)))
    (make-instance 'date-index :id month
                               :posts content
                               :title (format nil "Posts from ~a" month))))

(defun index-by-n (i posts &optional (step 10))
  "Return the index for the Ith page of POSTS in reverse chronological order."
  (make-instance 'int-index :id (1+ i)
                            :posts (let ((index (* step i)))
                                     (subseq posts index (min (length posts)
                                                              (+ index step))))
                            :title "Recent Posts"))

(defun render-indices ()
  "Render the indices to view posts in groups of size N, by month, and by tag."
  (let ((posts (by-date (find-all 'post))))
    (dolist (tag (all-tags))
      (let ((index (index-by-tag tag posts)))
        (write-page (page-path index) (render-page index))))
    (dolist (month (all-months))
      (let ((index (index-by-month month posts)))
        (write-page (page-path index) (render-page index))))
    (dotimes (i (ceiling (length posts) 10))
      (let ((index (index-by-n i posts)))
        (write-page (page-path index)
                    (render-page index nil
                                 :prev (and (plusp i) i)
                                 :next (and (< (* (1+ i) 10) (length posts))
                                            (+ 2 i)))))))
  (update-symlink "index.html" "1.html"))
