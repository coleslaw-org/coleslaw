(defpackage #:coleslaw-conf
  (:use #:cl)
  (:export #:*basedir*))

(in-package #:coleslaw-conf)

(defparameter *basedir*
  (uiop/pathname:pathname-parent-directory-pathname
   #.(or *compile-file-truename* *load-truename*))
  "A pathname pointing to Coleslaw's top level directory.")
