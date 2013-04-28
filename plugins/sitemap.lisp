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
                #:write-page)
  (:export #:enable))

(in-package :coleslaw-sitemap)

(defmethod deploy :before (staging)
  "Render sitemap.xml under document root"
  (let ((urls (append '("" "sitemap.xml") ; empty string is for root url
                      (mapcar #'page-url (find-all 'coleslaw:post)))))
    (write-page (rel-path (staging-dir *config*) "sitemap.xml")
                (funcall (theme-fn :sitemap "feeds")
                         (list :config *config*
                               :urls urls
                               :pubdate (make-pubdate))))))

(defun enable ())
