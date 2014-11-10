(defpackage :coleslaw-static-pages
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:*config*
                          #:content
                          #:find-all
                          #:render
                          #:publish
                          #:theme-fn
                          #:render-text
                          #:write-document))

(in-package :coleslaw-static-pages)

(defclass page (content)
  ((title :initarg :title :reader coleslaw::title-of)))

(defmethod initialize-instance :after ((object page) &key)
  ;; Expect all static-pages to be written in Markdown for now.
  (with-slots (coleslaw::url coleslaw::text) object
    (setf coleslaw::url (make-pathname :defaults coleslaw::url)
          coleslaw::text (render-text coleslaw::text :md))))

(defmethod render ((object page) &key next prev)
  ;; For the time being, we'll re-use the normal post theme.
  (declare (ignore next prev))
  (funcall (theme-fn 'post) (list :config *config*
                                  :post object)))

(defmethod publish ((doc-type (eql (find-class 'page))))
  (dolist (page (find-all 'page))
    (write-document page)))

(defun enable ())
