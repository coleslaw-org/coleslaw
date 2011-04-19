(in-package :coleslaw)

(defun monthlinks ()
  (loop for month in (gethash :months-index *storage*)
     collect (cons (index-url :date month) month) into months
     finally (return
               (pretty-list (mapcar (lambda (consp)
                                      (format nil "<a href=\"~a\">~a</a>"
                                              (car consp) (cdr consp)))
                                    months)))))

(defun taglinks ()
  (loop for tag in (gethash :tags-index *storage*)
     collect (cons (index-url :tag tag) tag) into tags
     finally (return
               (pretty-list (mapcar (lambda (consp)
                                      (format nil "<a href=\"~a\">~a</a>"
                                              (car consp) (cdr consp)))
                                    tags)))))

(defun index-title (id &optional page)
  (case id
    (:range "Recent Posts")
    (:date (format nil "Posts from ~A" page))
    (:tag (format nil "Posts tagged ~A" page))))

(defun index-posts (id page)
  (case id
    (:range (let* ((count (hash-table-count (gethash :posts *storage*)))
                   (start (- count (* 10 (1- page))))
                   (end (- start 9)))
              (remove nil (find-by-range start end))))
    (:date (find-by-date page))
    (:tag (find-by-tag page))))

(defmethod render-index (id page)
  (let* ((posts (index-posts id page))
         (content (funcall (find-symbol "INDEX" (theme-package))
                           (list :taglinks (taglinks)
                                 :monthlinks (monthlinks)
                                 :title (index-title id page)
                                 :posts (loop for post in posts collect
                                             (list :url (post-url (post-id post))
                                                   :title (post-title post)
                                                   :date (pretty-date (post-date post))
                                                   :contents (post-content post)))
                                 :prev (when (and (numberp id)
                                                  (index-posts id (1- page)))
                                         (index-url id (1- page)))
                                 :next (when (and (numberp id)
                                                  (index-posts id (1+ page)))
                                         (index-url id (1+ page)))))))
    content))

(defmethod index-url (id page)
  (flet ((keyword-name (keyword)
           (format nil "~A" keyword)))
    (if (member id '(:date :tag))
        (concatenate 'string *site-root* "/"
                     (string-downcase (keyword-name id)) "/" page)
        (concatenate 'string *site-root* "/page/" (write-to-string page)))))
