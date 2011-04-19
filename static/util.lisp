(in-package :coleslaw)

(defun pretty-date (date)
  (subseq (local-time:format-rfc1123-timestring nil date) 0 16))

(defun pretty-list (list)
  (format nil "~{~A~^, ~}" list))

(defun year-month (date)
  (format nil "~4d-~2,'0d" (local-time:timestamp-year date)
          (local-time:timestamp-month date)))

(defun theme-fn (name)
  (find-symbol name (theme-package)))
