(in-package :coleslaw)

(defclass post (content)
  ((title :initform nil :initarg :title :accessor post-title)
   (format :initform nil :initarg :format :accessor post-format)
   (content :initform nil :initarg :content :accessor post-content)))

(defmethod render ((object post) &key prev next)
  (funcall (theme-fn 'post) (list :config *config*
                                  :post object
                                  :prev prev
                                  :next next)))

(defmethod page-path ((object post))
  (rel-path (staging *config*) "posts/~a" (content-slug object)))

(defmethod initialize-instance :after ((object post) &key)
  (with-accessors ((title post-title)
                   (format post-format)
                   (content post-content)) object
      (setf (content-slug object) (slugify title)
            format (make-keyword (string-upcase format))
            content (render-content content format))))

(defmethod discover ((content-type (eql :post)))
  (purge-all 'post)
  (do-files (file (repo *config*) "post")
    (let ((post (construct :post (read-content file t))))
      (if (gethash (content-slug post) *content*)
          (error "There is already an existing post with the slug ~a."
                 (content-slug post))
          (setf (gethash (content-slug post) *content*) post)))))

(defmethod publish ((content-type (eql :post)))
  (loop for (next post prev) on (append '(nil) (by-date (find-all 'post)))
     while post do (write-page (page-path post)
                               (render-page post nil :prev prev :next next))))
