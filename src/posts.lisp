(in-package :coleslaw)

(defclass post (content)
  ((title :initform nil :initarg :title :accessor post-title)
   (author :initform nil :initarg :author :accessor post-author)
   (format :initform nil :initarg :format :accessor post-format)))

(defmethod initialize-instance :after ((object post) &key)
  (with-accessors ((title post-title)
                   (author post-author)
                   (format post-format)
                   (text content-text)) object
      (setf (content-slug object) (slugify title)
            format (make-keyword (string-upcase format))
            text (render-content text format)
            author (or author (author *config*)))))

(defmethod render ((object post) &key prev next)
  (funcall (theme-fn 'post) (list :config *config*
                                  :post object
                                  :prev prev
                                  :next next)))

(defmethod page-url ((object post))
  (format nil "~a/~a" (posts-dir *config*) (content-slug object)))

(defmethod publish ((doc-type (eql (find-class 'post))))
  (loop for (next post prev) on (append '(nil) (by-date (find-all 'post)))
     while post do (write-page (page-path post)
                               (render-page post nil :prev prev :next next))))
