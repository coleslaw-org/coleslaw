(defpackage :coleslaw-static-pages
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:*config*
                          #:assert-field
                          #:content
                          #:find-all
                          #:render
                          #:publish
                          #:theme-fn
                          #:render-text
                          #:write-document))

(in-package :coleslaw-static-pages)

(defclass page (content)
  ((title :initarg :title :reader coleslaw::title-of)
   (format :initarg :format :reader coleslaw::page-format))
  ;; default format is markdown (for backward compatibility)
  (:default-initargs :format :md))

(defmethod initialize-instance :after ((object page) &key)
  (assert-field 'title object)
  (assert-field 'coleslaw::url object)
  (with-slots (coleslaw::url coleslaw::text format title) object
    (setf coleslaw::url (make-pathname :defaults coleslaw::url)
          format (alexandria:make-keyword (string-upcase format))
          coleslaw::text (render-text coleslaw::text format))))

(defmethod render ((object page) &key next prev)
  ;; For the time being, we'll re-use the normal post theme.
  (declare (ignore next prev))
  (funcall (theme-fn 'post) (list :config *config*
                                  :post object)))

(defmethod publish ((doc-type (eql (find-class 'page))))
  (dolist (page (find-all 'page))
    (write-document page)))

(defun enable ())
