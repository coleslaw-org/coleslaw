(in-package :coleslaw)

;; TODO
;; How are we handling next/prev + ids?
;; Implement find-by-date.
;; Consider having find-by-range collect all the hash-values in :posts
;; and return the range of that list rather than the posts matching start->end.
;; Consider storing tags as a list.

(defmethod make-post :before (title tags date content &key id &allow-other-keys)
  (when id
    (let ((index (gethash :posts-index *storage* 0)))
      (unless (<= id index)
        (setf (gethash :posts-index *storage*) (1+ id))))))

(defmethod make-post (title tags date content &key id)
  (make-instance 'post :id (or id (gethash :posts-index *storage* 0))
                 :title title
                 :tags tags
                 :date date
                 :content content))

(defmethod add-post ((post post) id)
  (setf (gethash id (gethash :posts *storage*)) post))

(defmethod remove-post (id)
  (setf (gethash id (gethash :posts *storage*)) nil))

(defmethod render-post (id)
  (flet ((fn (name)
           (find-symbol name (theme-package))))
    (let* ((post (find-post id))
           (result (funcall (fn "POST")
                           (list :title (post-title post)
                                 :tags (post-tags post)
                                 :date (post-date post)
                                 :content (post-content post)
                                 :prev (post-prev post)
                                 :next (post-next post)))))
      result)))

(defmethod find-post (id)
  (gethash id (gethash :posts *storage*)))

(defmethod find-by-tag (tag)
  (let ((results nil))
    (loop for post being the hash-values in (gethash :posts *storage*)
       do (when (search tag (post-tags post))
            (push post results)))
    results))

(defmethod find-by-range (start end)
  (loop for i from start upto end
     collecting (find-post id) into results
     finally (return (remove nil results))))
