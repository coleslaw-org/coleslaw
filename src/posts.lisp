(in-package :coleslaw)

(defclass post (content)
  ((title  :initarg :title  :reader title-of)
   (author :initarg :author :reader author-of)
   (format :initarg :format :reader post-format))
  (:default-initargs :author nil))

(defmethod initialize-instance :after ((object post) &key)
  (with-slots (url title author format text) object
    (setf url (compute-url object (slugify title))
          format (make-keyword (string-upcase format))
          text (render-text text format)
          author (or author (author *config*)))))

(defmethod render ((object post) &key prev next)
  (funcall (get-theme-fn 'post (template-engine *config*))
           (list :config *config*
                 :post object
                 :prev prev
                 :next next)))

(defmethod publish ((doc-type (eql (find-class 'post))))
  (loop for (next post prev) on (append '(nil) (by-date (find-all 'post)))
     while post do (write-document post nil :prev prev :next next)))
