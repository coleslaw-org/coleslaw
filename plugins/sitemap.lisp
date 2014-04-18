(defpackage :coleslaw-sitemap
  (:use :cl)
  (:import-from :coleslaw
                #:*config*
                #:deploy
                #:find-all
                #:make-pubdate
                #:page-url
                #:rel-path
                #:staging-dir
                #:theme-fn
                #:write-file)
  (:import-from :local-time
		#:format-timestring
		#:now)
  (:export #:enable))

(in-package :coleslaw-sitemap)

(defmethod deploy :before (staging)
  "Render sitemap.xml under document root."
  (declare (ignore staging))
  (let ((urls (append '("" "sitemap.xml") ; empty string is for root url
                      (mapcar #'page-url (hash-table-values coleslaw::*site*)))))
    (write-file (rel-path (staging-dir *config*) "sitemap.xml")
                (funcall (theme-fn 'sitemap "sitemap")
                         (list :config *config*
                               :urls urls
                               :pubdate (format-timestring nil (now)))))))

(defun enable ())
