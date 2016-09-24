(defpackage :coleslaw-tests
  (:use :cl :prove))

(in-package :coleslaw-tests)

(plan 3)

(diag "COLESLAW-CONF:*BASEDIR* points to Coleslaw's top level directory")
(is (car (last (pathname-directory coleslaw-conf:*basedir*)))
    "coleslaw" :test #'string=)
(ok (probe-file (merge-pathnames #P"plugins" coleslaw-conf:*basedir*))
    "COLESLAW-CONF:*BASEDIR* has a plugins sub-directory")
(ok (probe-file (merge-pathnames #P"themes" coleslaw-conf:*basedir*))
    "COLESLAW-CONF:*BASEDIR* has a themes sub-directory")

(finalize)
