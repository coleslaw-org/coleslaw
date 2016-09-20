(eval-when (:compile-toplevel :load-toplevel)
  (asdf:load-system 'cl-who))

(defpackage :coleslaw-cl-who-renderer
  (:use #:cl #:cl-who)
  (:import-from #:coleslaw :render-text)
  (:export #:enable))

(in-package :coleslaw-cl-who-renderer)

(defmethod render-text (text (format (eql :cl-who)))
  (let* ((*package* (find-package '#:coleslaw-cl-who-renderer))
         (sexps (with-input-from-string (v text)
                  (do* ((line (read v)
                              (read v nil 'done))
                        (acc (list line)
                             (cons line acc)))
                       ((eql line 'done)
                        (nreverse (cdr acc)))))))
    (eval `(with-html-output-to-string (v) ,@sexps))))

(defun enable ())
