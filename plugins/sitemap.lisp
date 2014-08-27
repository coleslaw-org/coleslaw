(defpackage :coleslaw-sitemap
  (:use :cl)
  (:import-from :coleslaw #:*config*
                          #:index
                          #:page-url
                          #:find-all
                          #:publish
                          #:theme-fn
                          #:add-document
                          #:write-document)
  (:import-from :alexandria #:hash-table-values)
  (:export #:enable))

(in-package :coleslaw-sitemap)

(defclass sitemap ()
  ((urls :initarg :urls :reader urls)))

(defmethod page-url ((object sitemap)) "sitemap.xml")

;; We do 'discovery' in the publish method here because we can't ensure the
;; sitemap discover method is called last. Need all other content to be
;; discovered/in the DB.
(defmethod publish ((doc-type (eql (find-class 'sitemap))))
  (let* ((base-urls '("" "sitemap.xml"))
         (urls (mapcar #'page-url (hash-table-values coleslaw::*site*)))
         (sitemap (make-instance 'sitemap :urls (append base-urls urls))))
    (write-document sitemap (theme-fn 'sitemap "sitemap"))))

(defun enable ())
