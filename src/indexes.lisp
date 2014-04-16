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

(defmethod page-url ((object tag-index))
  (format nil "tag/~a" (index-slug object)))

(defmethod discover ((doc-type (eql (find-class 'tag-index))))
  (purge-all (class-name doc-type))
  (let ((content (by-date (find-all 'post))))
    (dolist (tag (all-tags))
      (add-document (index-by-tag tag content)))))

(defun index-by-tag (tag content)
  "Return an index of all CONTENT matching the given TAG."
  (make-instance 'tag-index :slug (tag-slug tag)
                 :content (remove-if-not (lambda (x) (tag-p tag x)) content)
                 :title (format nil "Posts tagged ~a" (tag-name tag))))

(defmethod publish ((doc-type (eql (find-class 'tag-index))))
  (dolist (index (find-all 'tag-index))
    (render-index index)))

;;; Index by Month

(defclass month-index (index) ())

(defmethod page-url ((object month-index))
  (format nil "date/~a" (index-slug object)))

(defmethod discover ((doc-type (eql (find-class 'month-index))))
  (purge-all (class-name doc-type))
  (let ((content (by-date (find-all 'post))))
    (dolist (month (all-months))
      (add-document (index-by-month month content)))))

(defun index-by-month (month content)
  "Return an index of all CONTENT matching the given MONTH."
  (make-instance 'month-index :slug month
                 :content (remove-if-not (lambda (x) (month-p month x)) content)
                 :title (format nil "Posts from ~a" month)))

(defmethod publish ((doc-type (eql (find-class 'month-index))))
  (dolist (index (find-all 'month-index))
    (render-index index)))

;;; Reverse Chronological Index

(defclass numeric-index (index) ())

(defmethod page-url ((object numeric-index))
  (format nil "~d" (index-slug object)))

(defmethod discover ((doc-type (eql (find-class 'numeric-index))))
  (purge-all (class-name doc-type))
  (let ((content (by-date (find-all 'post))))
    (dotimes (i (ceiling (length content) 10))
      (add-document (index-by-n i content)))))

(defun index-by-n (i content)
  "Return the index for the Ith page of CONTENT in reverse chronological order."
  (let ((content (subseq content (* 10 i))))
    (make-instance 'numeric-index :slug (1+ i)
                   :content (take-up-to 10 content)
                   :title "Recent Posts")))

(defmethod publish ((doc-type (eql (find-class 'numeric-index))))
  (let ((indexes (sort (find-all 'numeric-index) #'< :key #'index-slug)))
    (dolist (index indexes)
      (let ((prev (1- (index-slug index)))
            (next (1+ (index-slug index))))
        (render-index index :prev (when (plusp prev) prev)
                            :next (when (<= next (length indexes)) next))))))

;;; Atom and RSS Feeds

(defclass feed (index)
  ((format :initform nil :initarg :format :accessor feed-format)))

(defmethod page-url ((object feed))
  (format nil "~(~a~).xml" (feed-format object)))

(defmethod discover ((doc-type (eql (find-class 'feed))))
  (let ((content (take-up-to 10 (by-date (find-all 'post)))))
    (dolist (format '(rss atom))
      (let ((feed (make-instance 'feed :content content :format format)))
        (add-document feed)))))

(defmethod publish ((doc-type (eql (find-class 'feed))))
  (dolist (feed (find-all 'feed))
    (render-feed feed)))

(defclass tag-feed (feed) ())

(defmethod page-url ((object tag-feed))
  (format nil "tag/~a~(~a~).xml" (index-slug object) (feed-format object)))

(defmethod discover ((doc-type (eql (find-class 'tag-feed))))
  (let ((content (by-date (find-all 'post))))
    (dolist (tag (feeds *config*))
      (let ((posts (remove-if-not (lambda (x) (tag-p tag x)) content)))
        (dolist (format '(rss atom))
          (let ((feed (make-instance 'tag-feed :content (take-up-to 10 posts)
                                     :format format
                                     :slug tag)))
            (add-document feed)))))))

(defmethod publish ((doc-type (eql (find-class 'tag-feed))))
  (dolist (feed (find-all 'tag-feed))
    (render-feed feed)))

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

(defun render-feed (feed)
  "Render the given FEED to both RSS and ATOM."
  (let ((theme-fn (theme-fn (feed-format feed) "feeds")))
    (write-page (page-path feed) (render-page feed theme-fn))))

(defun render-index (index &rest render-args)
  "Render the given INDEX using RENDER-ARGS if provided."
  (write-page (page-path index) (apply #'render-page index nil render-args)))
