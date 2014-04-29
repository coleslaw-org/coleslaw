(in-package :coleslaw)

(defclass index ()
  ((slug :initform nil :initarg :slug :accessor index-slug)
   (title :initform nil :initarg :title :accessor index-title)
   (content :initform nil :initarg :content :accessor index-content)))

(defmethod render ((object index) &key prev next)
  (funcall (theme-fn 'index) (list :tags (all-tags)
                                   :months (all-months)
                                   :config *config*
                                   :index object
                                   :prev prev
                                   :next next)))

;;; Index by Tag

(defclass tag-index (index) ())

(defmethod discover ((doc-type (eql (find-class 'tag-index))))
  (let ((content (by-date (find-all 'post))))
    (dolist (tag (all-tags))
      (add-document (index-by-tag tag content)))))

(defun index-by-tag (tag content)
  "Return an index of all CONTENT matching the given TAG."
  (make-instance 'tag-index :slug (tag-slug tag)
                 :content (remove-if-not (lambda (x) (tag-p tag x)) content)
                 :title (format nil "Content tagged ~a" (tag-name tag))))

(defmethod publish ((doc-type (eql (find-class 'tag-index))))
  (dolist (index (find-all 'tag-index))
    (write-document index)))

;;; Index by Month

(defclass month-index (index) ())

(defmethod discover ((doc-type (eql (find-class 'month-index))))
  (let ((content (by-date (find-all 'post))))
    (dolist (month (all-months))
      (add-document (index-by-month month content)))))

(defun index-by-month (month content)
  "Return an index of all CONTENT matching the given MONTH."
  (make-instance 'month-index :slug month
                 :content (remove-if-not (lambda (x) (month-p month x)) content)
                 :title (format nil "Content from ~a" month)))

(defmethod publish ((doc-type (eql (find-class 'month-index))))
  (dolist (index (find-all 'month-index))
    (write-document index)))

;;; Reverse Chronological Index

(defclass numeric-index (index) ())

(defmethod discover ((doc-type (eql (find-class 'numeric-index))))
  (let ((content (by-date (find-all 'post))))
    (dotimes (i (ceiling (length content) 10))
      (add-document (index-by-n i content)))))

(defun index-by-n (i content)
  "Return the index for the Ith page of CONTENT in reverse chronological order."
  (let ((content (subseq content (* 10 i))))
    (make-instance 'numeric-index :slug (1+ i)
                   :content (take-up-to 10 content)
                   :title "Recent Content")))

(defmethod publish ((doc-type (eql (find-class 'numeric-index))))
  (let ((indexes (sort (find-all 'numeric-index) #'< :key #'index-slug)))
    (dolist (index indexes)
      (let ((prev (1- (index-slug index)))
            (next (1+ (index-slug index))))
        (write-document index nil
                        :prev (when (plusp prev) prev)
                        :next (when (<= next (length indexes)) next))))))

;;; Atom and RSS Feeds

(defclass feed (index)
  ((format :initform nil :initarg :format :accessor feed-format)))

(defmethod discover ((doc-type (eql (find-class 'feed))))
  (let ((content (by-date (find-all 'post))))
    (dolist (format '(rss atom))
      (let ((feed (make-instance 'feed :format format
                                 :content (take-up-to 10 content)
                                 :slug (format nil "~(~a~)" format))))
        (add-document feed)))))

(defmethod publish ((doc-type (eql (find-class 'feed))))
  (dolist (feed (find-all 'feed))
    (write-document feed (theme-fn (feed-format feed) "feeds"))))

;;; Tag Feeds

(defclass tag-feed (feed) ())

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

;;; Helper Functions

(defun all-months ()
  "Retrieve a list of all months with published content."
  (let ((months (mapcar (lambda (x) (subseq (content-date x) 0 7))
                        (find-all 'post))))
    (sort (remove-duplicates months :test #'string=) #'string>)))

(defun all-tags ()
  "Retrieve a list of all tags used in content."
  (let* ((dupes (mappend #'content-tags (find-all 'post)))
         (tags (remove-duplicates dupes :test #'string= :key #'tag-slug)))
    (sort tags #'string< :key #'tag-name)))
