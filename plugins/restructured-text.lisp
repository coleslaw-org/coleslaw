(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'docutils))

(defpackage :coleslaw-rst
  (:use :cl)
  (:import-from :coleslaw #:render-content)
  (:import-from :docutils #:read-rst
                          #:write-html)
  (:export #:enable))

(in-package :coleslaw-rst)

(defmethod render-content (text (format (eql :rst)))
  (with-output-to-string (str)
    (write-html str (read-rst text))))

(defun enable ())
