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

(defun render-feeds ()
  "Render and write the feed for the site."
  (let* ((bydate (by-date (hash-table-values *posts*)))
	 (posts (subseq bydate 0 (when (>= (length bydate) 10) 10)))
         (content (loop for post in posts
                     collect (list :title (post-title post)
                                   :url (post-url post)
                                   :date (make-pubdate (post-date post))
                                   :tags (post-tags post)
                                   :content (post-content post))))
         (tmpl-args (list :pubdate (make-pubdate)
                          :title (title *config*)
                          :siteroot (domain *config*)
                          :author (author *config*)
                          :posts content)))
    (render-page "rss.xml" (funcall (theme-fn 'rss) tmpl-args) :raw t)
    (render-page "feed.atom" (funcall (theme-fn 'atom) tmpl-args) :raw t)))
