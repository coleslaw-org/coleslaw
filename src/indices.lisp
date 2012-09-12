(in-package :coleslaw)

(defclass index ()
  ((path :initform nil :initarg :path :accessor index-path)
   (posts :initform nil :initarg :posts :accessor index-posts)
   (title :initform nil :initarg :title :accessor index-title)))

(defmethod render ((content index) &key prev next)
  (funcall (theme-fn 'index)
           (list :tags (all-tags)
                 :months (all-months)
                 :config *config*
                 :title (index-title index)
                 :posts (index-posts index)
                 :prev prev
                 :next next)))

(defun all-months ()
  "Retrieve a list of all months with published posts."
  (sort (remove-duplicates (mapcar (lambda (x) (get-month (post-date x)))
                                   (hash-table-values *posts*)) :test #'string=)
        #'string>))

(defun all-tags ()
  "Retrieve a list of all tags used in posts."
  (sort (reduce (lambda (x y) (union x y :test #'string=))
                (mapcar #'post-tags (hash-table-values *posts*)))
        #'string<))

(defun get-month (timestamp)
  "Extract the YYYY-MM portion of TIMESTAMP."
  (subseq timestamp 0 7))

(defun by-date (posts)
  "Sort POSTS in reverse chronological order."
  (sort posts #'string> :key #'post-date))

(defun render-by-n (posts &optional n)
  "Render the indices to view POSTS in reverse chronological order by N."
  (flet ((by-n (posts start)
           (let ((index (* n (1- start))))
             (subseq posts index (min (length posts) (+ index n))))))
    (loop for i = 1 then (1+ i)
       do (render-page (make-instance 'index :path (format nil "~d.html" i)
                                      :posts (by-n posts i)
                                      :title "Recent Posts")
                       nil
                       :prev (and (plusp (1- i)) (1- i))
                       :next (and (< (* i n) (length posts)) (1+ i)))
       until (> (* i n) (length posts))))
  (update-symlink "index.html" "1.html"))

(defun render-by-tag (posts tags)
  "Render the indices to view POSTS by tag for each tag in TAGS."
  (dolist (tag tags)
    (let ((posts (remove-if-not (lambda (post) (member tag (post-tags post)
                                                       :test #'string=)) posts)))
      (render-page (make-instance 'index :path (format nil "tag/~a.html" tag)
                                         :posts (by-date posts)
                                         :title (format nil "Posts tagged ~a" tag))))))

(defun render-by-month (posts months)
  "Render the indices to view POSTS by month for each month in MONTHS."
  (dolist (month months)
    (let ((posts (remove-if-not (lambda (post) (search month (post-date post))) posts)))
      (render-page (make-instance 'index :path (format nil "date/~a.html" month)
                                         :posts (by-date posts)
                                         :title (format nil "Posts from ~a" month))))))

(defun render-indices ()
  "Render the indices to view posts in groups of size N, by month, and by tag."
  (let ((posts (hash-table-values *posts*)))
    (render-by-n (by-date posts) 10)
    (render-by-tag posts (all-tags))
    (render-by-month posts (all-months))))
