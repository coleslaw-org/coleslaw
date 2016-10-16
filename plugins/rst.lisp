(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'docutils))

(defpackage :coleslaw-rst
  (:use :cl)
  (:import-from :coleslaw #:render-text)
  (:import-from :docutils
                #:document
                #:read-rst
                #:register-settings-spec
                #:visit-node
                #:write-document)
  (:import-from #:docutils.writer.html
                #:html-writer
                #:parts
                #:body-pre-docinfo
                #:body)
  (:export #:enable))

(in-package :coleslaw-rst)


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
