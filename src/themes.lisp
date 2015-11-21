(in-package :coleslaw)

(defparameter *injections* '()
  "A list that stores pairs of (string . predicate) to inject in the page.")

(defun add-injection (injection location)
  "Adds an INJECTION to a given LOCATION for rendering. The INJECTION should be a
function that takes a DOCUMENT and returns NIL or a STRING for template insertion."
  (push injection (getf *injections* location)))

(defun find-injections (content)
  "Iterate over *INJECTIONS* collecting any that should be added to CONTENT."
  (flet ((injections-for (location)
           (loop for injection in (getf *injections* location)
              collecting (funcall injection content))))
    (list :head (injections-for :head)
          :body (injections-for :body))))

(define-condition theme-does-not-exist (error)
  ((theme :initarg :theme :reader theme))
  (:report (lambda (c stream)
             (format stream "Cannot find the theme: '~A'" (theme c)))))

(defun theme-package (name)
  "Find the package matching the theme NAME or signal THEME-DOES-NOT-EXIST."
  (or (find-package (format nil "~:@(coleslaw.theme.~A~)" name))
      (error 'theme-does-not-exist :theme name)))

(defun theme-fn (name &optional (package (theme *config*)))
  "Find the symbol NAME inside PACKAGE which defaults to the theme package."
  (find-symbol (princ-to-string name) (theme-package package)))

(defun find-theme (theme)
  "Find the theme prefering themes in the local repo."
  (let ((local-theme (repo-path "themes/~a/" theme)))
    (if (probe-file local-theme)
        local-theme
        (app-path "themes/~a/" theme))))

(defun compile-theme (theme)
  "Locate and compile the templates for the given THEME."
  (do-files (file (find-theme theme) "tmpl")
    (compile-template :common-lisp-backend file))
  (do-files (file (app-path "themes/") "tmpl")
    (compile-template :common-lisp-backend file)))
