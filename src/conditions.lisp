(in-package :coleslaw)

(define-condition plugin-conf-error ()
  ((plugin :initform "Plugin":initarg :plugin :reader plugin)
   (message :initform "" :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream "~A: ~A" (plugin condition) (message condition))))
  (:documentation "Condition to signal when the plugin is misconfigured."))

