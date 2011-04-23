(in-package :coleslaw)

(defparameter *current-theme* "hyde"
  "The name of a directory containing templates for HTML generation.")

(defparameter *theme-dir* (merge-pathnames
                           (concatenate 'string "themes/" *current-theme* "/")
                           (asdf:system-source-directory 'coleslaw))
  "The directory containing the current theme and other site templates.")

(defgeneric add-injection (str location)
  (:documentation "Add STR to the list of elements injected in LOCATION."))

(defgeneric remove-injection (str location)
  (:documentation "Remove STR from the list of elements injected in LOCATION."))

(defun theme-package (&key (name *current-theme*))
  (find-package (string-upcase (concatenate 'string "coleslaw.theme." name))))

(defun compile-theme (&key (theme-dir *theme-dir*))
  (loop for file in (cl-fad:list-directory theme-dir) do
       (let ((extension (pathname-type file)))
         (when (and extension (string= extension "tmpl"))
           (compile-template :common-lisp-backend file)))))

;; DOCUMENTATION
;; A theme directory should be named after the theme and contain *.tmpl files
;; that define the following functions in a coleslaw.theme.$NAME namespace.
;; Required templates follow with suggested args (args will be in plists):
;; {template base} // title navigation siteroot content credits &key license headInject bodyInject
;; {template post} // title tags date content prev next &key comments
;; {template index} // title posts prev next &key taglinks monthlinks count
