(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :cl-closure))

(defpackage :coleslaw-cl-closure
  (:use :cl)
  (:import-from :closure-template #:compile-template)
  (:import-from :local-time #:format-rfc1123-timestring)
  (:import-from :coleslaw #:find-theme
                          #:app-path
                          #:render
                          #:find-injections
                          #:*config*
                          #:theme
                          #:do-files
                          #:render
                          #:theme-package)
  (:export #:enable))

(in-package :coleslaw-cl-closure)

(defclass cl-closure () ())

(defmethod coleslaw:compile-theme ((cl-closure cl-closure) theme)
  "Compile the templates used by cl-closure"
  (do-files (file (app-path "themes/auxiliary")
                  "cl-closure")
    (compile-template :common-lisp-backend file))
  (do-files (file (find-theme theme) "tmpl")
    (compile-template :common-lisp-backend file)))

(defmethod coleslaw:render-page ((template cl-closure) content &optional theme-fn &rest render-args)
  (apply (or theme-fn (get-theme-fn 'theme 'base))
         :config *config*
         :content content
         :pubdate (format-rfc1123-timestring nil (local-time:now))
         :injections (find-injections content)
         :raw (apply #'render content render-args)
         render-args))

(defmethod coleslaw:get-theme-fn ((template cl-closure) name &optional (package (theme *config*)))
  (let ((closure-func (find-symbol (princ-to-string name) (theme-package package))))
    (lambda (&rest template-args)
      (funcall closure-func template-args))))

(defun enable ())
