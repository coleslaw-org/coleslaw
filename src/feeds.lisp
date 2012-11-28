(in-package :coleslaw)

(defun make-pubdate (&optional date)
  "Make a RFC1123 pubdate representing the current time or DATE, when supplied."
  (let ((timestamp (if date
                       (date-to-timestamp date)
                       (local-time:now))))
    (local-time:format-rfc1123-timestring nil timestamp)))

(defun render-feeds (feeds)
  "Render and write the given FEEDS for the site."
  (flet ((first-10 (list)
           (subseq list 0 (min (length list) 10))))
    (let* ((by-date (by-date (hash-table-values *posts*)))
           (posts (first-10 by-date))
           (rss (make-instance 'index :id "rss.xml" :posts posts))
           (atom (make-instance 'index :id "feed.atom" :posts posts)))
      (write-page (page-path rss) (render-page rss :rss-feed))
      (write-page (page-path atom) (render-page atom :atom-feed))
      (dolist (feed feeds)
        (let ((index (index-by-tag feed by-date)))
          (setf (index-id index) (format nil "tag/~a.xml" feed)
                (index-posts index) (first-10 (index-posts index)))
          (write-page (page-path index) (render-page index :rss-feed)))))))
