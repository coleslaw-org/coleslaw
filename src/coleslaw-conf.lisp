(defpackage #:coleslaw-conf
  (:use #:cl)
  (:export #:*basedir*
           #:set-basedir))

(in-package #:coleslaw-conf)

(defvar *basedir*)

(defun set-basedir (pathname)
  (setf coleslaw-conf:*basedir*
    (make-pathname :name nil :type nil :defaults pathname)))
