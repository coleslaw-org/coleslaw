(defpackage :coleslaw-tests
  (:use :cl :prove))

(in-package :coleslaw-tests)

(plan 4)

(diag "COLESLAW-CONF:*BASEDIR* points to Coleslaw's top level directory")
(is (car (last (pathname-directory coleslaw-conf:*basedir*)))
    "coleslaw" :test #'string=)
(ok (probe-file (merge-pathnames #P"plugins" coleslaw-conf:*basedir*))
    "COLESLAW-CONF:*BASEDIR* has a plugins sub-directory")
(ok (probe-file (merge-pathnames #P"themes" coleslaw-conf:*basedir*))
    "COLESLAW-CONF:*BASEDIR* has a themes sub-directory")


(coleslaw::load-config (asdf:system-relative-pathname :coleslaw-test "tests/files/"))

(with-open-file (in (asdf:system-relative-pathname :coleslaw-test "tests/files/127.txt"))
  (diag "PARSE-METADATA should handle files with CR-LF line endings.")
  (is (coleslaw::parse-metadata in) '(:TITLE "We should handle CR-LF" :TAGS "fixtures" :DATE "2014-12-16" :FORMAT
 "md" :EXCERPT "An excerpt") :test 'equalp))

(finalize)
