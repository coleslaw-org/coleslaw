(defpackage :coleslaw-static-pages
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:*config*
                          #:content
                          #:content-text
                          #:page-url
                          #:find-all
                          #:render
                          #:publish
                          #:theme-fn
                          #:render-text
                          #:write-document))

(in-package :coleslaw-static-pages)

(defclass page (content)
  ((title :initarg :title :reader title-of)
   (url :initarg :url :reader page-url)))

(defmethod initialize-instance :after ((object page) &key)
  ;; Expect all static-pages to be written in Markdown for now.
  (with-accessors ((text content-text)) object
    (setf text (render-text text :md))))

(defmethod render ((object page) &key next prev)
  ;; For the time being, we'll re-use the normal post theme.
  (declare (ignore next prev))
  (funcall (theme-fn 'post) (list :config *config*
                                  :post object)))

(defmethod publish ((doc-type (eql (find-class 'page))))
  (dolist (page (find-all 'page))
    (write-document page)))

(defun enable ())
