(in-package :coleslaw)

(defmethod make-post (title tags date content &key id aliases)
  (make-instance 'post :id (incf (gethash :posts-index *storage* 0))
                 :title title
                 :tags tags
                 :date date
                 :content content
                 :aliases aliases))

(defmethod find-post (id)
  (gethash id (gethash :posts *storage*)))

(defun (setf find-post) (new-val id)
  (setf (gethash id (gethash :posts *storage*)) new-val)
  new-val)

(defmethod add-post ((post post) id)
  (setf (find-post id) post)
  (add-to-index "recent" post)
  (loop for tag in (post-tags post) do
       (pushnew tag (gethash :tags-list *storage*) :test #'string=)
       (add-to-index (concatenate 'string "tag/" tag) post))
  (let ((year-month (year-month (post-date post))))
    (pushnew (year-month (post-date post))
             (gethash :months-list *storage*) :test #'string=)
    (add-to-index (concatenate 'string "date/" year-month) post)))

(defmethod remove-post (id)
  ;; Removes post from storage and indexes but not disk! Should we support more?
  (let ((post (find-post id)))
    (loop for tag in (post-tags post) do
         (remove-from-index (concatenate 'string "tag/" tag) post))
    (remove-from-index (concatenate 'string "date/"
                                    (month-year (post-date post))))
    (setf (find-post id) nil)))

(defmethod render-post (id)
  (let* ((post (find-post id))
         (result (funcall (theme-fn "POST")
                          (list :title (post-title post)
                                :tags (pretty-list (post-tags post))
                                :date (pretty-date (post-date post))
                                :content (post-content post)
                                :prev (when (find-post (1- id))
                                        (post-url (1- id)))
                                :next (when (find-post (1+ id))
                                        (post-url (1+ id)))))))
    result))

(defmethod post-url (id)
  (let ((post (find-post id)))
    (concatenate 'string *site-root* "/"
                 (year-month (post-date post)) "/"
                 (escape (post-title post)))))
