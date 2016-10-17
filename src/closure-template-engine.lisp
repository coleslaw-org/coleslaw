(in-package #:coleslaw)

(defmethod compile-theme (theme (template-engine (eql :cl-closure)))
  "Locate and compile the templates for the given THEME."
  (do-files (file (find-theme theme) "tmpl")
    (compile-template :common-lisp-backend file))
  (do-files (file (app-path "themes/") "tmpl")
    (compile-template :common-lisp-backend file)))

(defmethod get-theme-fn (name (template-engine (eql :cl-closure)) &optional (package (theme *config*)))
  (theme-fn name package))
