(defpackage #:coleslaw-pygments
  (:use #:cl)
  (:export #:enable))

(in-package #:coleslaw-pygments)

(defun enable ()
  (setf 3bmd-code-blocks:*renderer* :pygments))
