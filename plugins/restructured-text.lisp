(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'docutils))

(defpackage :coleslaw-rst
  (:use :cl :coleslaw))

(in-package :coleslaw-rst)
