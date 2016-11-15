(in-package #:cl-user)

;; Code for generating some files in tests/files/

(defun write-with-cr-lf (line stream)
  (format stream line)
  (format stream "~A~A" #\Return #\Linefeed))

(with-open-file (out (asdf:system-relative-pathname :coleslaw-test "tests/files/127.txt") :direction :output :if-exists :overwrite)
  (write-with-cr-lf ";;;;;" out)
  (write-with-cr-lf "title: We should handle CR-LF" out)
  (write-with-cr-lf "tags: fixtures" out)
  (write-with-cr-lf "date: 2014-12-16" out)
  (write-with-cr-lf "format: md" out)
  (write-with-cr-lf ";;;;;" out))
