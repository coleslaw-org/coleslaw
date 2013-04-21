(in-package :coleslaw)

(defun make-pubdate ()
  "Make a RFC1123 pubdate representing the current time."
  (local-time:format-rfc1123-timestring nil (local-time:now)))

(defun render-feeds (feeds)
  "Render and write the given FEEDS for the site."
  (flet ((first-10 (list)
           (subseq list 0 (min (length list) 10))))
    (let* ((by-date (by-date (find-all 'post)))
           (posts (first-10 by-date))
           (rss (make-instance 'index :id "rss.xml" :posts posts))
           (atom (make-instance 'index :id "feed.atom" :posts posts))
           (rss-template (theme-fn :rss-feed "feeds"))
           (atom-template (theme-fn :atom-feed "feeds")))
      (write-page (page-path rss) (render-page rss rss-template))
      (write-page (page-path atom) (render-page atom atom-template))
      (dolist (feed feeds)
        (let ((index (index-by-tag (make-tag feed) by-date)))
          (setf (index-id index) (format nil "~a-rss.xml" feed)
                (index-posts index) (first-10 (index-posts index)))
          (write-page (page-path index) (render-page index rss-template)))))))
