(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'cl-who))

(defpackage :coleslaw-cl-who
  (:use #:cl #:cl-who)
  (:import-from #:coleslaw #:render-text)
  (:export #:enable))

(in-package :coleslaw-cl-who)


(defmethod render-text (text (format (eql :cl-who)))
  (with-output-to-string (o)
    (with-input-from-string (s (prog2 (in-package :cl-who);kludge
                                      (concatenate 'string
                                                   (format nil "(cl-who:with-html-output-to-string (o) (htm~%")
                                                   coleslaw-cl-who::text
                                                   (format nil "~%))"))
                                      (in-package :coleslaw-cl-who))) ;bugfix for kludge
      (format o "~a" (eval (read s nil))))))

(defun enable ())
