(in-package :coleslaw)

(defclass post (content)
  ((title :initarg :title :reader title-of)
   (author :initarg :author :accessor author-of)
   (format :initarg :format :accessor post-format))
  (:default-initargs :author nil))

(defmethod initialize-instance :after ((object post) &key)
  (with-accessors ((title title-of)
                   (author author-of)
                   (format post-format)
                   (text content-text)) object
      (setf (content-slug object) (slugify title)
            format (make-keyword (string-upcase format))
            text (render-text text format)
            author (or author (author *config*)))))

(defmethod render ((object post) &key prev next)
  (funcall (theme-fn 'post) (list :config *config*
                                  :post object
                                  :prev prev
                                  :next next)))

(defmethod publish ((doc-type (eql (find-class 'post))))
  (loop for (next post prev) on (append '(nil) (by-date (find-all 'post)))
     while post do (write-document post nil :prev prev :next next)))
