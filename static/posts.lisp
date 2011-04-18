(in-package :coleslaw)

(defmethod make-post (title tags date content &key id aliases)
  (make-instance 'post :id (incf (gethash :posts-index *storage* 0))
                 :title title
                 :tags tags
                 :date date
                 :content content
                 :aliases aliases))

(defmethod add-post ((post post) id)
  (setf (gethash id (gethash :posts *storage*)) post)
  (loop for tag in (post-tags post)
     do (pushnew tag (gethash :tags-index *storage*) :test #'string=)))

(defmethod remove-post (id)
  (setf (gethash id (gethash :posts *storage*)) nil))

(defmethod render-post (id)
  (flet ((fn (name)
           (find-symbol name (theme-package)))
         (pretty-date (date)
           (subseq (local-time:format-rfc1123-timestring nil date) 0 16))
         (pretty-tags (tags)
           (format nil "~{~A~^, ~}" tags)))
    (let* ((post (find-post id))
           (result (funcall (fn "POST")
                           (list :title (post-title post)
                                 :tags (pretty-tags (post-tags post))
                                 :date (pretty-date (post-date post))
                                 :content (post-content post)
                                 :prev (when (find-post (1- id))
                                         (post-url (1- id)))
                                 :next (when (find-post (1+ id))
                                         (post-url (1+ id)))))))
      result)))

(defmethod find-post (id)
  (gethash id (gethash :posts *storage*)))

(defmethod find-by-tag (tag)
  (let ((results nil))
    (loop for post being the hash-values in (gethash :posts *storage*)
       do (when (search tag (post-tags post))
            (push post results)))
    results))

(defmethod find-by-date (date)
  (let ((results nil)
        (year (parse-integer (subseq date 0 4)))
        (month (parse-integer (subseq date 5))))
    (loop for post being the hash-values in (gethash :posts *storage*)
       do (let ((post-date (post-date post)))
            (when (and (= year (local-time:timestamp-year post-date))
                       (= month (local-time:timestamp-month post-date)))
              (push post results))))
    results))

(defmethod find-by-range (start end)
  (if (> start end)
      (loop for id from start downto end collecting (find-post id))
      (loop for id from start upto end collecting (find-post id))))

(defmethod post-url (id)
  (flet ((escape (str)
           (substitute #\- #\Space str)))
    (concatenate 'string *site-root* (escape (post-title (find-post id))))))
