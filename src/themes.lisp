(in-package :coleslaw)

(defparameter *injections* (make-hash-table :test #'equal))

(defgeneric add-injection (str location)
  (:documentation "Add STR to the list of elements injected in LOCATION.")
  (:method ((str string) location)
    (pushnew str (gethash location *injections*) :test #'string=)))

(defgeneric remove-injection (str location)
  (:documentation "Remove STR from the list of elements injected in LOCATION.")
  (:method ((str string) location)
    (setf (gethash location *injections*)
          (remove str (gethash location *injections*) :test #'string=))))

(defun theme-package (&key (name (theme *config*)))
  (find-package (string-upcase (concatenate 'string "coleslaw.theme." name))))

(defun compile-theme (&key (theme-dir (app-path "themes/~a/" (theme *config*))))
  (do-files (file theme-dir "tmpl")
    (compile-template :common-lisp-backend file)))

;; DOCUMENTATION
;; A theme directory should be named after the theme and contain *.tmpl files
;; that define the following functions in a coleslaw.theme.$NAME namespace.
;; Required templates follow with suggested args (args will be in plists):
;; {template base} // title navigation siteroot content credits &key license headInject bodyInject
;; {template post} // title tags date content prev next &key comments
;; {template index} // title posts prev next &key taglinks monthlinks count
