(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :djula))

(defpackage #:coleslaw-djula
  (:use #:cl)
  (:import-from #:djula
                #:compile-template*
                #:render-template*)
  (:import-from #:coleslaw
                #:*config*
                #:get-theme-fn
                #:compile-theme
                #:template-engine
                ;; Internal
                #:do-files
                #:app-path
                #:find-theme
                #:theme
                #:theme-fn
                #:theme-package
                #:ensure-theme-package)
  (:export #:enable
           #:*template-file-extension*
           #:*template-store*))

(in-package #:coleslaw-djula)

(defvar *template-file-extension* "djula"
  "The file extension used for the templates.")

(defvar *template-store* (make-hash-table :test #'equalp)
  "Store the compiled templates using the file name as keys.")

(defun compile-template (template-file)
  "Compile the djula template and store the compiled template for later use."
  (let ((template-name (intern (string-upcase (pathname-name template-file))
                               (theme-package (theme *config*)))))
    (setf (gethash template-name *template-store*)
          (compile-template* template-file))))

(defmethod compile-theme (theme (template-engine (eql :djula)))
  ;; Process the base files and the themes. The base files should be processed in the enable
  (do-files (template-file (find-theme theme)
                           *template-file-extension*)
    (compile-template template-file)))

(defmethod get-theme-fn (name (template-engine (eql :djula)) &optional package)
  (declare (ignore package))
  (let* ((package (theme *config*))
         (template (gethash (theme-fn name package)
                           *template-store*)))
    (unless template
      (error "Template ~A not found" name))
    (lambda (template-args)
      (with-output-to-string (out)
        (apply #'render-template* template out template-args)))))

(defun enable ()
  ;; Set the template-engine to djula
  (setf (template-engine *config*)
        :djula)
  (ensure-theme-package (theme *config*))
  ;; Compile the base templates
  (do-files (template-file (app-path "themes/base-djula/")
                           *template-file-extension*)
    (compile-template template-file)))
