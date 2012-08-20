(in-package :coleslaw)

(defun taglinks ()
  (let ((tags (remove-duplicates (mapcar #'post-tags *posts*))))
    (loop for tag in tags
       collect (list :url (format nil "~a/tag/~a.html" (domain *config*) tag)
                     :name tag))))

(defun monthlinks ()
  (let ((months (mapcar (lambda (x) (get-month (post-date x))) *posts*)))
    (loop for month in months
       collect (list :url (format nil "~a/month/~a.html" (domain *config*) month)
                     :name month))))

(defun write-index (posts filename title)
  (let ((content (loop for post in posts
                    collect (list :url (format nil "~a/posts/~a.html"
                                               (domain *config*) (post-slug post))
                                  :title (post-title post)
                                  :date (post-date post)
                                  :contents (post-contents post)))))
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
  (flet ((by-20 (posts start)
           (let ((index (* 20 (1- start))))
             (subseq posts index (min (length posts) (+ index 19))))))
    (let ((posts (sort *posts* #'string> :key #'post-date)))
      (loop for i from 1 then (1+ i)
         until (> (* (1- i) 20) (length posts))
         do (write-index (by-20 posts i) (format nil "~d.html" i) "Recent Posts")))))

(defun render-by-tag ()
  (let ((tags (remove-duplicates (mapcan #'post-tags *posts*) :test #'string=)))
    (loop for tag in tags
       do (flet ((match-tag (post)
                   (member tag post :test #'string= :key #'post-tags)))
            (let ((posts (remove-if-not #'match-tag posts)))
              (write-index posts (format nil "tag/~a.html" tag)
                           (format nil "Posts tagged ~a" tag)))))))

(defun render-by-month ()
  (let ((months (remove-duplicates (mapcar (lambda (x) (subseq (post-date x) 0 7))
                                           *posts*) :test #'string=)))
    (loop for month in months
       do (let ((posts (remove-if-not (lambda (x) (search month (post-date x))
                                              *posts*))))
            (write-index posts (format nil "date/~a.html" (subseq month 0 7))
                         (format nil "Posts from ~a" (subseq month 0 7)))))))

(defun render-indices ()
  (render-by-20)
  (render-by-tag)
  (render-by-month))
