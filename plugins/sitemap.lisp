(defpackage :coleslaw-sitemap
  (:use :cl)
  (:import-from :coleslaw #:*config*
                          #:index
                          #:page-url
                          #:find-all
                          #:discover
                          #:publish
                          #:theme-fn
                          #:add-document
                          #:write-document)
  (:import-from :alexandria #:hash-table-values)
  (:export #:enable))

(in-package :coleslaw-sitemap)

(defclass sitemap (index)
  ((urls :initarg :urls :reader urls)))

(defmethod page-url ((object sitemap)) "sitemap.xml")

(defmethod discover ((doc-type (eql (find-class 'sitemap))))
  (let ((base-urls '("" "sitemap.xml"))
        (urls (mapcar #'page-url (hash-table-values coleslaw::*site*))))
    (add-document (make-instance 'sitemap :urls (append base-urls urls)))))

(defmethod publish ((doc-type (eql (find-class 'sitemap))))
  (dolist (sitemap (find-all 'sitemap))
    (write-document sitemap (theme-fn 'sitemap "sitemap"))))

(defun enable ())
