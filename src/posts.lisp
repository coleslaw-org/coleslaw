(in-package :coleslaw)

(defclass post (content)
  ((title  :initarg :title  :reader title-of)
   (author :initarg :author :reader author-of)
   (format :initarg :format :reader post-format)
   (keywords :initarg :keywords :reader keywords-of)
   (description :initarg :description :reader description-of)
   (image :initarg :image :reader image-of)
   (card :initarg :card :reader card-format)
   (creator :initarg :creator :reader creator-of))
  (:default-initargs
   :author nil
   :keywords nil
   :description nil
   :image nil
   :card nil
   :creator nil))

(defmethod initialize-instance :after ((object post) &key)
  (with-slots (url title author format card text) object
    (setf url (compute-url object (slugify title))
          format (make-keyword (string-upcase format))
          text (render-text text format)
          author (or author (author *config*))
          card (if card (make-keyword (string-upcase card))))))

(defmethod render ((object post) &key prev next)
  (funcall (theme-fn 'post) (list :config *config*
                                  :post object
                                  :prev prev
                                  :next next)))

(defmethod publish ((doc-type (eql (find-class 'post))))
  (loop for (next post prev) on (append '(nil) (by-date (find-all 'post)))
     while post do (write-document post nil :prev prev :next next)))
