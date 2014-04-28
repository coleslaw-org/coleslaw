(defpackage :coleslaw-static-pages
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:content
                          #:page-url
                          #:find-all
                          #:render
                          #:publish))

(in-package :coleslaw-static-pages)

(defclass page (content)
  ((:url :initarg :url :reader page-url)))

(defmethod render ((object page))
  ;; For now, we'll re-use the normal post theme.
  (funcall (theme-fn 'post) (list :config *config*
                                  :post object)))

(defmethod publish ((doc-type (eql (find-class 'page))))
  (dolist (page (find-all 'page))
    (write-file (page-path page) (render-page page nil))))

(defun enable ())
