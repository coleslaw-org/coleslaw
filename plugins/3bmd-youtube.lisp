(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :3bmd-youtube))

(defpackage #:coleslaw-3bmd-youtube
  (:use #:cl)
  (:export
   #:enable))

(in-package #:coleslaw-3bmd-youtube)

(defun enable ()
  (setf 3bmd-youtube:*youtube-embeds* t))
