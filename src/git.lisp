(in-package :coleslaw)

(defun last-commit ()
  "Retrieve the SHA1 hash of the most recent blog commit."
  (multiple-value-bind (pid stdout stderr)
      (with-current-directory (repo *config*)
        (run-program "git" '("log" "-n 1")))
    (cl-ppcre:scan-to-strings "[0-9a-f]{40}" stdout)))

(defun last-published ()
  "Retrieve the SHA1 hash of the most recent published blog."
  (with-open-file (in "/home/redline/.coleslaw" :if-does-not-exist :create)
    (read-line in nil)))

(defun (setf last-published) (new-val)
  (with-open-file (out "/home/redline/.coleslaw"
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (write-line new-val out)))

(defun blog-update-p ()
  "Returns a non-nil value if the blog needs to be regenerated."
  (mismatch (last-commit) (last-published)))
