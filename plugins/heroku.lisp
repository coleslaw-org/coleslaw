(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload '(hunchentoot)))

(defpackage :coleslaw-heroku
  (:use :cl)
  (:import-from #:hunchentoot :create-folder-dispatcher-and-handler
                              :create-static-file-dispatcher-and-handler
                              :*dispatch-table*)
  (:import-from #:coleslaw :deploy
                           :*config*)
  (:export #:enable))

(in-package :coleslaw-heroku)

(defmethod deploy :after (staging)
  (let ((blog (merge-pathnames ".curr/" (deploy *config*))))
    (push (create-folder-dispatcher-and-handler "/" blog)
          *dispatch-table*)
    (push (create-static-file-dispatcher-and-handler "/" (merge-pathnames "index.html" blog))
          *dispatch-table*)))

(defun enable ())
