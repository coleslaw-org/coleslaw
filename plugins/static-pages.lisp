(defpackage :coleslaw-static-pages
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:*config*
                          #:content
                          #:page-url
                          #:find-all
                          #:render
                          #:publish
                          #:write-document))

(in-package :coleslaw-static-pages)

(defclass page (content)
  ((url :initarg :url :reader page-url)))

(defmethod render ((object page) &key next prev)
  ;; For now, we'll re-use the normal post theme.
  (funcall (theme-fn 'post) (list :config *config*
                                  :post object)))

(defmethod publish ((doc-type (eql (find-class 'page))))
  (dolist (page (find-all 'page))
    (write-document page)))

(defun enable ())
