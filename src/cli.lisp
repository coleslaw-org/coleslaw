(defpackage #:coleslaw-cli
  (:use :cl)
  (:documentation "CLI processing tools.")
  (:export
   #:process-parameters))

(in-package :coleslaw-cli)

(defun process-parameters (argv)
  "Return an alist where the key is the option name and the value the
  value. Only accept long form options."
  (unless (evenp (length argv))
    (error "All Options take a parameter."))
  (labels ((%iter (argv alist)
             (cond ((null argv) alist)
                   (t
                    (%iter (cddr argv)
                           (acons (alexandria:make-keyword
                                   (string-upcase
                                    (string-trim "-" (first argv))))
                                  (second argv)
                                  alist))))))
    (%iter argv nil)))
