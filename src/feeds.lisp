(in-package :coleslaw)

(defun date-to-timestamp (date)
  "Convert a post DATE to a local-time timestamp."
  (destructuring-bind (date time) (cl-ppcre:split " " date)
    (apply 'local-time:encode-timestamp 0
           (mapcar #'parse-integer
                   (append (reverse (cl-ppcre:split ":" time))
                           (reverse (cl-ppcre:split "-" date)))))))

(defun make-pubdate (&optional date)
  "Make a RFC1123 pubdate representing the current time or DATE, when supplied."
  (let ((timestamp (if date
                       (date-to-timestamp date)
                       (local-time:now))))
    (local-time:format-rfc1123-timestring nil timestamp)))

(defun render-feeds (feeds)
  "Render and write the given FEEDS for the site."
  (let* ((by-date (by-date (hash-table-values *posts*)))
         (posts (subseq by-date 0 (min (length by-date) 10))))
    (render-page (make-instance 'index :path "rss.xml" :posts posts) :rss)
    (render-page (make-instance 'index :path "feed.atom" :posts posts) :atom)
    (dolist (feed feeds)
      (let ((index (index-by-tag feed by-tag)))
        (setf (index-path index) (format nil "tag/~a-rss.xml" feed))
        (render-page index :rss)))))
