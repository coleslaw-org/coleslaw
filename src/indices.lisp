(in-package :coleslaw)

(defun all-months ()
  "Retrieve a list of all months with published posts."
  (remove-duplicates (mapcar (lambda (x) (get-month (post-date x)))
                             (hash-table-values *posts*)) :test #'string=))

(defun all-tags ()
  "Retrieve a list of all tags used in posts."
  (reduce (lambda (x y) (union x y :test #'string=))
          (mapcar #'post-tags (hash-table-values *posts*))))

(defun taglinks (&optional (tags (all-tags)))
  "Generate links to all the tag indices or those in TAGS."
  (loop for tag in (sort tags #'string<)
     collect (list :url (format nil "tag/~a.html" tag) :name tag)))

(defun monthlinks (&optional (months (all-months)))
  "Generate links to all the month indices."
  (loop for month in (sort months #'string<)
     collect (list :url (format nil "date/~a.html" month) :name month)))

(defun get-month (timestamp)
  "Extract the YYYY-MM portion of TIMESTAMP."
  (subseq timestamp 0 7))

(defun by-date (posts)
  "Sort POSTS in reverse chronological order."
  (sort posts #'string> :key #'post-date))

(defun write-index (posts filename title &key prev next (relative t))
  "Write out the HTML for POSTS to FILENAME.html."
  (let ((content (loop for post in posts
                    collect (list :url (if relative
                                           (format nil "../posts/~a" (post-url post))
                                           (format nil "~a/posts/~a"
                                                   (domain *config*) (post-url post)))
                                  :title (post-title post)
                                  :date (post-date post)
                                  :content (render-content (post-content post)
                                                           (post-format post))))))
    (render-page filename
                 (funcall (theme-fn "INDEX")
                          (list :taglinks (taglinks)
                                :monthlinks (monthlinks)
                                :siteroot (domain *config*)
                                :title title
                                :posts content
                                :prev (and prev (format nil "~d.html" prev))
                                :next (and next (format nil "~d.html" next)))))))

(defun render-by-n (&optional n)
  "Render the indices to view posts in reverse chronological order by N."
  (flet ((by-n (posts start)
           (let ((index (* n (1- start))))
             (subseq posts index (min (length posts) (+ index n))))))
    (let ((posts (by-date (hash-table-values *posts*))))
      (loop for i = 1 then (1+ i)
         do (write-index (by-n posts i) (format nil "~d.html" i) "Recent Posts"
                         :prev (and (plusp (1- i)) (1- i))
                         :next (and (< (* i n) (length posts)) (1+ i))
                         :relative nil)
         until (> (* i n) (length posts)))
      (update-symlink "index.html" "1.html"))))

(defun render-by-tag (tags)
  "Render the indices to view posts by tag for each tag in TAGS."
  (dolist (tag tags)
    (let ((posts (remove-if-not (lambda (post) (member tag (post-tags post)) :test #'string=)
                                (hash-table-values *posts*))))
      (write-index (by-date posts)
                   (format nil "tag/~a.html" tag)
                   (format nil "Posts tagged ~a" tag)))))

(defun render-by-month (months)
  "Render the indices to view posts by month for each month in MONTHS."
  (dolist (month months)
    (let ((posts (remove-if-not (lambda (post) (search month (post-date post)))
                                (hash-table-values *posts*))))
      (write-index (by-date posts)
                   (format nil "date/~a.html" month)
                   (format nil "Posts from ~a" month)))))

(defun render-indices ()
  "Render the indices to view posts in groups of size N, by month, and by tag."
  (render-by-n 10)
  (render-by-tag (all-tags))
  (render-by-month (all-months)))
