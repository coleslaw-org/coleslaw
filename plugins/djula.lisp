(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :djula))

(defpackage :coleslaw-djula
  (:use :cl :cl-ppcre)
  (:import-from :djula #:compile-template*
                       #:render-template*)
  (:import-from :local-time #:format-rfc1123-timestring)
  (:import-from :coleslaw #:get-theme-fn
                          #:render-page
                          #:find-theme
                          #:app-path
                          #:render
                          #:find-injections
                          #:*config*
                          #:theme
                          #:do-files)
  (:import-from :cl-ppcre #:create-scanner
                          #:regex-replace)
  (:export #:enable))

(in-package :coleslaw-djula)

(defclass djula ()
  ((templates :initform (make-hash-table :test #'equalp) :accessor templates)))

(defparameter +remove-extension+ (create-scanner "(.*)\\..*"))

(defun compile-djula (djula file &optional (filename (regex-replace +remove-extension+ (file-namestring file) "\\1")))
  (setf (gethash filename
                 (templates djula)) (compile-template* file)))

(defmethod coleslaw:compile-theme ((djula djula) theme)
  (do-files (file (app-path "themes/auxiliary")
                  "djula")
    (compile-djula djula file))
  (do-files (file (find-theme theme) "html")
    (compile-djula djula file)))

(defmethod coleslaw:render-page ((djula djula) content &optional theme-fn &rest render-args)
  (apply (or theme-fn #'render)
         content
         :config *config*
         :content content
         :pubdate (format-rfc1123-timestring nil (local-time:now))
         :injections (find-injections content)
         render-args))

(defmethod coleslaw:get-theme-fn ((djula djula) name &optional package)
  (declare (ignore package))
  (break)
  (let ((theme (gethash (string name) (templates djula))))
    (lambda (&rest template-args)
      (with-output-to-string (stream)
        (apply #'render-template* theme stream template-args)))))

(defun enable ())
