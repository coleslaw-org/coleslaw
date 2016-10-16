(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'docutils))

(defpackage :coleslaw-rst
  (:use :cl)
  (:import-from :coleslaw #:render-text)
  (:import-from :docutils #:read-rst #:write-part #:register-settings-spec
                          #:visit-node #:write-document #:document)
  (:import-from :docutils.writer.html #:html-writer
                #:body-pre-docinfo
                #:body
                #:parts)
  (:export #:enable))

(in-package :coleslaw-rst)

;; XXX: This is not an ideal solution as it affects other uses of docutils in
;; the same lisp image.
(defmethod visit-node :after ((writer html-writer) (document document))
  "This method removes unnecessary HTML elements, such as html, head, body
and make docutils output only html fragment with document itself."
  (setf (slot-value writer 'parts) '(body-pre-docinfo
                                     body)))

(defmethod render-text (text (format (eql :rst)))
  (register-settings-spec '((:generator nil)
                            (:datestamp nil)))
  (let ((writer (make-instance 'html-writer))
        (document (read-rst text)))
    (write-document writer document 'string)))

(defun enable ())
