(defpackage :coleslaw-tests
  (:use :cl :prove))

(in-package :coleslaw-tests)

(plan nil)

(deftest 1-is-a-number
  (is-type 1 'fixnum))

(finalize)
