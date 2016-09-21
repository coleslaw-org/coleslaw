(defpackage :coleslaw-tests
  (:use :cl :prove))

(in-package :coleslaw-tests)

(plan 1)

(diag "COLESLAW-CONF:*BASEDIR* points to Coleslaw's top level directory")
(is (car (last (pathname-directory coleslaw-conf:*basedir*)))
    "coleslaw" :test #'string=)

(finalize)
