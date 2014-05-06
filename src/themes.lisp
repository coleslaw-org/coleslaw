(in-package :coleslaw)

(defparameter *injections* '()
  "A list that stores pairs of (string . predicate) to inject in the page.")

(defun add-injection (injection location)
  "Adds an INJECTION to a given LOCATION for rendering. The INJECTION should be
a string which will always be added or a (string . lambda). In the latter case,
the lambda takes a single argument, a content object, i.e. a POST or INDEX, and
any return value other than nil indicates the injection should be added."
  (let ((result (etypecase injection
                  (string (list injection #'identity))
                  (list injection))))
    (push result (getf *injections* location))))

(defun find-injections (content)
  "Iterate over *INJECTIONS* collecting any that should be added to CONTENT."
  (flet ((injections-for (location)
           (loop for (injection predicate) in (getf *injections* location)
              when (funcall predicate content)
              collect injection)))
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

(defun compile-theme (theme)
  "Locate and compile the templates for the given THEME."
  (do-files (file (app-path "themes/~a/" theme) "tmpl")
    (compile-template :common-lisp-backend file))
  (do-files (file (app-path "themes/") "tmpl")
    (compile-template :common-lisp-backend file)))
