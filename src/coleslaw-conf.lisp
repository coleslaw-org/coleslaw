(defpackage #:coleslaw-conf
  (:use #:cl)
  (:import-from #:uiop/pathname
                #:pathname-parent-directory-pathname)
  (:export #:*basedir*))

(in-package #:coleslaw-conf)

(defparameter *basedir* (pathname-parent-directory-pathname (make-pathname :name nil :type nil :defaults *load-truename*))
  "A pathname pointing to Coleslaw's top level directory.")
