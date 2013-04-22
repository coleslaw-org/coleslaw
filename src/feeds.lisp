(in-package :coleslaw)

(defun make-pubdate ()
  "Make a RFC1123 pubdate representing the current time."
  (local-time:format-rfc1123-timestring nil (local-time:now)))

(defun render-feed (posts &key path template tag)
  (flet ((first-10 (list) (subseq list 0 (min (length list) 10)))
         (tag-posts (list) (remove-if-not (lambda (x) (tag-p tag x)) list)))
    (let ((template (theme-fn template "feeds"))
          (index (if tag
                     (make-instance 'tag-index :id path
                                    :posts (first-10 (tag-posts posts)))
                     (make-instance 'index :id path
                                    :posts (first-10 posts)))))
      (write-page (page-path index) (render-page index template)))))

(defun render-feeds (tag-feeds)
  "Render the default RSS and ATOM feeds along with any TAG-FEEDS."
  (let ((posts (by-date (find-all 'post))))
    (dolist (feed '((:path "rss.xml" :template :rss-feed)
                    (:path "feed.atom" :template :atom-feed)))
      (apply #'render-feed posts feed))
    (dolist (feed tag-feeds)
      (apply #'render-feed posts (list :path (format nil "~A-rss.xml" feed)
                                       :tag (make-tag feed)
                                       :template :rss-feed)))))
