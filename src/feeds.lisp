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
           (posts (first-10 by-date)))
      (render-page (make-instance 'index :path "rss.xml" :posts posts) :rss-feed)
      (render-page (make-instance 'index :path "feed.atom" :posts posts) :atom-feed)
      (dolist (feed feeds)
        (let ((index (index-by-tag feed by-date)))
          (setf (index-path index) (format nil "tag/~a-rss.xml" feed)
                (index-posts index) (first-10 (index-posts index)))
          (render-page index :rss))))))
