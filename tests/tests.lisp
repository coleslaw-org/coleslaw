(in-package :coleslaw-tests)

(defmacro deftest (name docstring &body body)
  `(test ,name
         ,docstring
         ,@body))

(def-suite coleslaw-tests)
(in-suite coleslaw-tests)

(deftest sanity-test
    "A blog should compile and deploy correctly."
  (is (zerop (coleslaw:main))))
