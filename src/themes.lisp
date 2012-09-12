(in-package :coleslaw)

(defparameter *injections* (make-hash-table :test #'equal)
  "A hash table for storing JS to inject in the theme templates.")

(defgeneric add-injection (str location)
  (:documentation "Add STR to the list of elements injected in LOCATION.")
  (:method ((str string) location)
    (pushnew str (gethash location *injections*) :test #'string=)))

(defun theme-package (&key (name (theme *config*)))
  "Find the package matching the theme NAME."
  (find-package (string-upcase (concatenate 'string "coleslaw.theme." name))))

(defun theme-fn (name)
  "Find the symbol NAME inside the current theme's package."
  (find-symbol (princ-to-string name) (theme-package)))

(defun compile-theme (theme)
  "Locate and compile the templates for the given THEME."
  (do-files (file (app-path "themes/~a/" theme) "tmpl")
    (compile-template :common-lisp-backend file))
  (do-files (file (app-path "themes/") "tmpl")
    (compile-template :common-lisp-backend file)))

;; DOCUMENTATION
;; A theme directory should be named after the theme and contain *.tmpl files
;; that define the following functions in a coleslaw.theme.$NAME namespace.
;; Required templates:
;; {template base}
;; {template post}
;; {template index}

