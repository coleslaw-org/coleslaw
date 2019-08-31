(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'cl-markless-plump))

(defpackage #:coleslaw-markless
  (:use #:cl)
  (:export #:enable))
(in-package :coleslaw-markless)

(defmethod render-text (text (format (eql :mess)))
  (cl-markless:output text :target NIL :format 'cl-markless-plump:plump))

(defun enable ())
