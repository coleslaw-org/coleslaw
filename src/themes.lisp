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

(defun find-theme (theme)
  "Find the theme prefering themes in the local repo."
  (let ((local-theme (repo-path "themes/~a/" theme)))
    (if (probe-file local-theme)
        local-theme
        (app-path "themes/~a/" theme))))

(defun theme-package-name (theme-name)
  (format nil "~:@(coleslaw.theme.~A~)" theme-name))

(defun make-theme-package (theme-name)
  (make-package (theme-package-name theme-name)))

(defun ensure-theme-package (theme-name)
  (handler-case (theme-package theme-name)
    (theme-does-not-exist (c) (make-theme-package (theme c)))))

(defun theme-package (theme-name)
  "Find the package matching the theme NAME or signal THEME-DOES-NOT-EXIST."
  (or (find-package (theme-package-name theme-name))
      (error 'theme-does-not-exist :theme theme-name)))

(defun theme-fn (name &optional (package (theme *config*)))
  "Find the symbol NAME inside PACKAGE which defaults to the theme package."
  (find-symbol (princ-to-string name) (theme-package package)))

(defgeneric get-theme-fn (name template-engine &optional package)
  (:documentation "Return the theme function to be used by RENDER-PAGE."))

(defgeneric compile-theme (theme template-engine)
  (:documentation "Locate and compile the templates for the given THEME."))
