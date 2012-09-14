(in-package :coleslaw)

(defparameter *injections* ()
  "A list that stores pairs of (string . predicate) to inject in the page.")

(defun add-injection (injection location)
  (push injection (getf *injections* location)))

(defun find-injections (content)
  (flet ((injections-for (location)
           (loop for (injection . predicate) in (getf *injections* location)
              when (funcall predicate content)
              collect injection)))
    (list :head (injections-for :head)
          :body (injections-for :body))))

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
