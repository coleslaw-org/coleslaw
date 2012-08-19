(in-package :coleslaw)

(defgeneric add-injection (str location)
  (:documentation "Add STR to the list of elements injected in LOCATION."))

(defgeneric remove-injection (str location)
  (:documentation "Remove STR from the list of elements injected in LOCATION."))

(defun theme-package (&key (name (theme *config*)))
  (find-package (string-upcase (concatenate 'string "coleslaw.theme." name))))

(defun compile-theme (&key (theme-dir (app-path (theme *config*))))
  (loop for file in (iolib.os:list-directory theme-dir)
     do (let ((extension (pathname-type file)))
          (when (and extension (string= extension "tmpl"))
            (compile-template :common-lisp-backend file)))))

;; DOCUMENTATION
;; A theme directory should be named after the theme and contain *.tmpl files
;; that define the following functions in a coleslaw.theme.$NAME namespace.
;; Required templates follow with suggested args (args will be in plists):
;; {template base} // title navigation siteroot content credits &key license headInject bodyInject
;; {template post} // title tags date content prev next &key comments
;; {template index} // title posts prev next &key taglinks monthlinks count
