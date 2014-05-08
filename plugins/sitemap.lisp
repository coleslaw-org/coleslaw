(defpackage :coleslaw-sitemap
  (:use :cl)
  (:import-from :coleslaw #:*config*
                          #:index
                          #:deploy
                          #:page-url
                          #:theme-fn
                          #:write-document)
  (:import-from :alexandria #:hash-table-values)
  (:export #:enable))

(in-package :coleslaw-sitemap)

(defclass sitemap (index)
  ((urls :initarg :urls :reader urls)))

(defmethod page-url ((object sitemap)) "sitemap.xml")

(defmethod deploy :before (staging)
  "Render sitemap.xml under document root."
  (declare (ignore staging))
  (let* ((urls (mapcar #'page-url (hash-table-values coleslaw::*site*)))
         (sitemap (make-instance 'sitemap :urls (append '("" "sitemap.xml") urls))))
    (write-document sitemap (theme-fn 'sitemap "sitemap"))))

(defun enable ())
