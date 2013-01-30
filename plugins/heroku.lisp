(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'hunchentoot))

(defpackage :coleslaw-heroku
  (:use :cl)
  (:import-from #:hunchentoot :create-folder-dispatcher-and-handler
                              :create-static-file-dispatcher-and-handler
                              :*dispatch-table*)
  (:import-from #:coleslaw :deploy)
  (:export #:enable))

(in-package :coleslaw-heroku)

(defmethod deploy :after (staging)
  (push (create-folder-dispatcher-and-handler "/" "/app/.curr/")
        *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler "/" "/app/.curr/index.html")
        *dispatch-table*))

(defun enable ())
