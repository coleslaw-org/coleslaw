(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'cl-who))

(defpackage :coleslaw-cl-who
  (:use #:cl #:cl-who)
  (:import-from #:coleslaw :render-text)
  (:export #:enable))

(in-package :coleslaw-cl-who)

(defmethod render-text (text (format (eql :cl-who)))
  (let* ((*package* (find-package "COLESLAW-CL-WHO"))
         (sexps (with-input-from-string (v text)
                  (do* ((line (read v)
                              (read v nil 'done))
                        (acc (list line)
                             (cons line acc)))
                       ((eql line 'done)
                        (nreverse (cdr acc)))))))
    (eval `(with-html-output-to-string (v) ,@sexps))))

(defun enable ()
  )
