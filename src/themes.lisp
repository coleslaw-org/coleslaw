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

(defun get-djula-theme (name)
  (symbol-value (intern (string-upcase (format nil "*djula-~A-template*" (string name))))))

(defun theme-fn (name &optional
                        (package (theme *config*))
                        (templating-engine *templating-engine*))
  "Find the symbol NAME inside PACKAGE which defaults to the theme package."
  (case templating-engine
    (cl-closure (let ((func (find-symbol (princ-to-string name)
                                         (theme-package package))))
                  (lambda (&rest rest)
                    (funcall func rest))))
    (djula (lambda (&rest rest)
             (with-output-to-string (stream)
               (apply #'render-template*
                      (get-djula-theme name)
                      stream
                      rest))))
    (otherwise (error "Unkown templating engine found"))))

(defun find-theme (theme)
  "Find the theme prefering themes in the local repo."
  (let ((local-theme (repo-path "themes/~a/" theme)))
    (if (probe-file local-theme)
        local-theme
        (app-path "themes/~a/" theme))))

(defun compile-theme (theme)
  "Locate and compile the templates for the given THEME."
  ;; Do stuff for atom and rss first
  (do-files (file (app-path "themes/") "tmpl")
    (compile-template :common-lisp-backend file))
  ;; Now find the correct templates
  (let ((theme-base (find-theme theme)))
    (format t "~A, ~A, ~A ~A~%" *templating-engine* (equal *templating-engine* (intern "DJULA"))
            (eql *templating-engine* (intern "DJULA"))
            (equal *templating-engine* (list 'DJULA)))
    (case *templating-engine*
      (cl-closure (do-files (file theme-base "tmpl")
                    (compile-template :common-lisp-backend file)))
      (djula (dolist (page '("post" "index"))
               (set (intern (string-upcase (format nil "*djula-~A-template*" page)))
                    (compile-template* (merge-pathnames theme-base
                                                        (make-pathname :directory "/"
                                                                       :name page
                                                                       :type "html"))))))
      (otherwise (error "Unkown templating engine found at compiling"))
      )))
