(in-package :coleslaw)

(defclass index ()
  ((slug :initform nil :initarg :slug :accessor index-slug)
   (posts :initform nil :initarg :posts :accessor index-posts)
   (title :initform nil :initarg :title :accessor index-title)))

(defclass tag-index (index) ())
(defclass date-index (index) ())
(defclass numeric-index (index) ())

(defmethod page-url ((object index))
  (index-id object))
(defmethod page-url ((object tag-index))
  (format nil "tag/~a" (index-id object)))
(defmethod page-url ((object date-index))
  (format nil "date/~a" (index-id object)))
(defmethod page-url ((object numeric-index))
  (format nil "~d" (index-id object)))

(defmethod render ((object index) &key prev next)
  (funcall (theme-fn 'index) (list :tags (all-tags)
                                   :months (all-months)
                                   :config *config*
                                   :index object
                                   :prev prev
                                   :next next)))

(defun all-months ()
  "Retrieve a list of all months with published content."
  (let ((months (mapcar (lambda (x) (subseq (content-date x) 0 7))
                        (hash-table-values *content*))))
    (sort (remove-duplicates months :test #'string=) #'string>)))

(defun all-tags ()
  "Retrieve a list of all tags used in content."
  (let* ((dupes (mappend #'content-tags (hash-table-values *content*)))
         (tags (remove-duplicates dupes :test #'string= :key #'tag-slug)))
    (sort tags #'string< :key #'tag-name)))

(defun index-by-tag (tag content)
  "Return an index of all CONTENT matching the given TAG."
  (make-instance 'tag-index :slug (tag-slug tag)
                 :posts (remove-if-not (lambda (x) (tag-p tag x)) content)
                 :title (format nil "Posts tagged ~a" (tag-name tag))))

(defun index-by-month (month content)
  "Return an index of all CONTENT matching the given MONTH."
  (make-instance 'date-index :slug month
                 :posts (remove-if-not (lambda (x) (month-p month x)) content)
                 :title (format nil "Posts from ~a" month)))

(defun index-by-n (i content &optional (step 10))
  "Return the index for the Ith page of CONTENT in reverse chronological order."
  (let* ((start (* step i))
         (end (min (length content) (+ start step))))
    (make-instance 'numeric-index :slug (1+ i)
                              :posts (subseq content start end)
                              :title "Recent Posts")))

(defun render-index (index &rest render-args)
  "Render the given INDEX using RENDER-ARGS if provided."
  (write-page (page-path index) (apply #'render-page index nil render-args)))

(defun render-indexes ()
  "Render the indexes to view content in groups of size N, by month, and by tag."
  (let ((results (by-date (find-all 'post))))
    (dolist (tag (all-tags))
      (render-index (index-by-tag tag results)))
    (dolist (month (all-months))
      (render-index (index-by-month month results)))
    (dotimes (i (ceiling (length results) 10))
      (render-index (index-by-n i results)
                    :prev (and (plusp i) i)
                    :next (and (< (* (1+ i) 10) (length results))
                               (+ 2 i))))))
