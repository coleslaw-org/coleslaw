(defpackage :coleslaw-sitemap 
  (:use :cl)
  (:import-from :coleslaw
                #:*config*
                #:deploy
                #:domain
                #:find-all
                #:page-url
                #:rel-path
                #:staging-dir
                #:theme-fn
                #:write-page)
  (:export #:enable))

(in-package :coleslaw-sitemap) 

(defmethod deploy :before (staging)
  "Render sitemap.xml under document root"
  (let* ((urls (append '("" "sitemap.xml") ; empty string is for root url
                       (mapcar #'page-url (find-all 'coleslaw:post)))))
    (write-page (rel-path (staging-dir *config*) "sitemap.xml")
                (funcall (theme-fn :sitemap "feeds")
                         (list :domain (domain *config*)
                               :urls urls
                               :pubdate (local-time:format-rfc3339-timestring
                                          nil
                                          (local-time:now))))))) 

(defun enable ())
