(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'docutils))

(defpackage :coleslaw-rst
  (:use :cl)
  (:import-from :coleslaw #:render-text)
  (:import-from :docutils #:read-rst #:write-part #:register-settings-spec
                          #:visit-node #:write-document)
  (:import-from :docutils.writer.html #:html-writer)
  (:export #:enable))

(in-package :coleslaw-rst)

(defmethod render-text (text (format (eql :rst)))
  (register-settings-spec '((:generator nil)
                            (:datestamp nil)))
  (with-output-to-string (str)
    (let ((writer (make-instance 'html-writer))
          (document (read-rst text)))
      (write-document writer document str))))

(defun enable ())
