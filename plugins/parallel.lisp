(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'lparallel))

(defpackage :coleslaw-parallel
  (:use :cl)
  (:export #:enable))

(in-package :coleslaw-parallel)

;; TODO: The bulk of the speedup here should come from parallelizing discover.
;; Publish will also benefit. Whether it's better to spin off threads for each
;; content type/index type or the operations *within* discover/publish is not
;; known, the higher granularity of doing it at the iterating over types level
;; is certainly easier to prototype though.

(defun enable ())
