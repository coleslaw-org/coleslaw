(in-package :coleslaw)

(define-condition required-field-missing (error)
  ((message :accessor required-field-missing-message :initarg :message))
  (:report
   (lambda (c s)
     (format s (required-field-missing-message
	      c)))))
