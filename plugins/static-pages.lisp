(defpackage :coleslaw-static-pages
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:*config*
                          #:content
                          #:find-all
                          #:render
                          #:publish
                          #:get-theme-fn
                          #:render-text
                          #:write-document
                          #:template-engine)
  (:import-from :djula #:render-template*))

(in-package :coleslaw-static-pages)

(defclass page (content)
  ((title :initarg :title :reader coleslaw::title-of)
   (format :initarg :format :reader coleslaw::page-format))
  ;; default format is markdown (for backward compatibility)
  (:default-initargs :format :md))

(defmethod initialize-instance :after ((object page) &key)
  (with-slots (coleslaw::url coleslaw::text format) object
    (setf coleslaw::url (make-pathname :defaults coleslaw::url)
          format (alexandria:make-keyword (string-upcase format))
          coleslaw::text (render-text coleslaw::text format))))

(defmethod render ((object page) &rest rest &key next prev)
  ;; For the time being, we'll re-use the normal post theme.
  (declare (ignore next prev))
  (apply (get-theme-fn (template-engine *config*) 'post) :config *config*
                          :post object
                          rest))

(defmethod publish ((doc-type (eql (find-class 'page))))
  (dolist (page (find-all 'page))
    (write-document page)))

(defun enable ())
