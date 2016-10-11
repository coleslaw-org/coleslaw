(in-package #:coleslaw)

(defmethod compile-theme (theme (template-engine (eql :cl-closure)))
  "Locate and compile the templates for the given THEME."
  (do-files (file (find-theme theme) "tmpl")
    (compile-template :common-lisp-backend file))
  (do-files (file (app-path "themes/") "tmpl")
    (compile-template :common-lisp-backend file)))

(defun theme-package (name)
  "Find the package matching the theme NAME or signal THEME-DOES-NOT-EXIST."
  (or (find-package (format nil "~:@(coleslaw.theme.~A~)" name))
      (error 'theme-does-not-exist :theme name)))

(defun theme-fn (name &optional (package (theme *config*)))
  "Find the symbol NAME inside PACKAGE which defaults to the theme package."
  (find-symbol (princ-to-string name) (theme-package package)))

(defmethod get-theme-fn (name (template-engine (eql :cl-closure)) &optional (package (theme *config*)))
  (theme-fn name package))
