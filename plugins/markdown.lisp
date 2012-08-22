(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(3bmd 3bmd-ext-code-blocks)))

(defpackage :coleslaw-md
  (:use :cl :coleslaw))

(in-package :coleslaw-md)

(defmethod render-content (text (format (eql :md)))
  (let ((3bmd-code-blocks:*code-blocks* t))
    (with-output-to-string (str)
      (3bmd:parse-string-and-print-to-stream text str))))
