(in-package :coleslaw)

;;; Atom and RSS Feeds

(defclass feed () ((format :initarg :format :reader feed-format)))

(defclass standard-feed (index feed) ())

(defmethod discover ((doc-type (eql (find-class 'standard-feed))))
  (let ((content (by-date (find-all 'post))))
    (dolist (format '(rss atom))
      (let ((feed (make-instance 'standard-feed :format format
                                 :content (take-up-to 10 content)
                                 :slug (format nil "~(~a~)" format))))
        (add-document feed)))))

(defmethod publish ((doc-type (eql (find-class 'standard-feed))))
  (dolist (feed (find-all 'standard-feed))
    (write-document feed (theme-fn (feed-format feed) "feeds"))))

;;; Tag Feeds

(defclass tag-feed (index feed) ())

(defmethod discover ((doc-type (eql (find-class 'tag-feed))))
  (let ((content (by-date (find-all 'post))))
    (dolist (tag (feeds *config*))
      (let ((tagged (remove-if-not (lambda (x) (tag-p tag x)) content)))
        (dolist (format '(rss atom))
          (let ((feed (make-instance 'tag-feed :format format
                                     :content (take-up-to 10 tagged)
                                     :slug (format nil "~a-~(~a~)" tag format))))
            (add-document feed)))))))

(defmethod publish ((doc-type (eql (find-class 'tag-feed))))
  (dolist (feed (find-all 'tag-feed))
    (write-document feed (theme-fn (feed-format feed) "feeds"))))
