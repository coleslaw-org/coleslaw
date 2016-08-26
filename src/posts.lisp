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

(defmethod render ((object post) &rest rest)
  (apply (get-theme-fn (template-engine *config*) 'post)
         :config *config*
         :post object
         rest))

(defmethod publish ((doc-type (eql (find-class 'post))))
  (loop for (next post prev) on (append '(nil) (by-date (find-all 'post)))
     while post do (write-document post nil :prev prev :next next)))
