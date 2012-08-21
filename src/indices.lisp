(in-package :coleslaw)

(defun all-months ()
  "Retrieve a list of all months with published posts."
  (remove-duplicates (mapcar (lambda (x) (subseq (post-date x) 0 7))
                             (hash-table-values *posts*)) :test #'string=))

(defun all-tags ()
  "Retrieve a list of all tags used in posts."
  (reduce (lambda (x y) (union x y :test #'string=))
          (mapcar #'post-tags (hash-table-values *posts*))))

(defun taglinks ()
  "Generate links to all the tag indices."
  (loop for tag in (all-tags)
     collect (list :url (format nil "~a/tag/~a.html" (domain *config*) tag)
                   :name tag)))

(defun monthlinks ()
  "Generate links to all the month indices."
  (loop for month in (all-months)
     collect (list :url (format nil "~a/date/~a.html" (domain *config*) month)
                   :name month)))

(defun write-index (posts filename title)
  "Write out the HTML for POSTS to FILENAME.html."
  (let ((content (loop for post in posts
                    collect (list :url (format nil "~a/posts/~a.html"
                                               (domain *config*) (post-slug post))
                                  :title (post-title post)
                                  :date (post-date post)
                                  :content (render-content (post-content post)
                                                           (post-format post))))))
    (render-page filename
                 (funcall (theme-fn "INDEX")
                          (list :taglinks (taglinks)
                                :monthlinks (monthlinks)
                                :title title
                                :posts content
                                ; TODO: Populate prev and next with links.
                                :prev nil
                                :next nil)))))

(defun render-by-20 ()
  "Render the indices to view posts in reverse chronological order by 20."
  (flet ((by-20 (posts start)
           (let ((index (* 20 (1- start))))
             (subseq posts index (min (length posts) (+ index 20))))))
    (let ((posts (sort (hash-table-values *posts*) #'string> :key #'post-date)))
      (loop for i = 1 then (1+ i)
         do (write-index (by-20 posts i) (format nil "~d.html" i) "Recent Posts")
         until (> (* i 20) (length posts))))))

(defun render-by-tag ()
  "Render the indices to view posts by tag."
  (loop for tag in (all-tags)
     do (flet ((match-tag (post)
                 (member tag (post-tags post) :test #'string=)))
          (let ((posts (remove-if-not #'match-tag (hash-table-values *posts*))))
            (write-index posts (format nil "tag/~a.html" tag)
                         (format nil "Posts tagged ~a" tag))))))

(defun render-by-month ()
  "Render the indices to view posts by month."
  (let ((months (remove-duplicates (mapcar (lambda (x) (subseq (post-date x) 0 7))
                                           (hash-table-values *posts*))
                                   :test #'string=)))
    (loop for month in months
       do (let ((posts (remove-if-not (lambda (x) (search month (post-date x)))
                                      (hash-table-values *posts*))))
            (write-index posts (format nil "date/~a.html" (subseq month 0 7))
                         (format nil "Posts from ~a" (subseq month 0 7)))))))

(defun render-indices ()
  "Render the indices to view posts in groups of 20, by month, and by tag."
  (render-by-20)
  (render-by-tag)
  (render-by-month))
