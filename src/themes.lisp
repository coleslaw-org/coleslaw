(in-package :coleslaw)

(defparameter *injections* '()
  "A list that stores pairs of (string . predicate) to inject in the page.")

(defclass template-engine () ())

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

(defun get-djula-theme (name)
  (symbol-value (intern (string-upcase (format nil "*djula-~A-template*" (string name))))))

(defgeneric get-theme-fn (template name &optional package)
  (:documentation "Get the function used to render the template for NAME.
The engine used to do this must be specified in TEMPLATE and should be a class
instance of an engine. PACKAGE can be used to specify where to search for
methods to render a template."))

(defun find-theme (theme)
  "Find the theme prefering themes in the local repo."
  (let ((local-theme (repo-path "themes/~a/" theme)))
    (if (probe-file local-theme)
        local-theme
        (app-path "themes/~a/" theme))))

(defgeneric compile-theme (template-engine theme)
  (:documentation "Locate and compile the templates for the given THEME and
  compile them. The compiling is done appropriately for each template engine
  that is specified in TEMPLATE-ENGINE in the form of a template engine class
  instance."))
