(in-package :coleslaw)

(defun make-pubdate ()
  "Make a RFC1123 pubdate representing the current time."
  (local-time:format-rfc1123-timestring nil (local-time:now)))

(defun first-10 (list)
  "Get up to the first 10 items in LIST."
  (subseq list 0 (min (length list) 10)))

(defun make-tag-feed (tag posts)
  "Make an RSS feed for the given TAG and POSTS."
  (flet ((valid-p (obj) (member tag (content-tags obj) :test #'tag-slug=)))
    (make-instance 'tag-index :id (format nil "~A-rss.xml" (tag-slug tag))
                   :posts (first-10 (remove-if-not #'valid-p posts)))))

(defun render-feed (posts &key path template tag)
  "Given a PATH, TEMPLATE, and possibly a TAG, render the appropriate feed."
  (let ((template (theme-fn template "feeds"))
        (index (if tag
                   (make-tag-feed tag posts)
                   (make-instance 'index :id path
                                  :posts (first-10 posts)))))
    (write-page (page-path index) (render-page index template))))

(defun render-feeds (tag-feeds)
  "Render the default RSS and ATOM feeds along with any TAG-FEEDS."
  (let ((posts (by-date (find-all 'post))))
    (dolist (feed '((:path "rss.xml" :template :rss-feed)
                    (:path "feed.atom" :template :atom-feed)))
      (apply #'render-feed posts feed))
    (dolist (feed tag-feeds)
      (apply #'render-feed posts (list :tag (make-tag feed)
                                       :template :rss-feed)))))
