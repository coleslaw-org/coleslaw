(in-package :coleslaw)

(defun pretty-date (date)
  (subseq (local-time:format-rfc1123-timestring nil date) 0 16))

(defun pretty-list (list)
  (format nil "~{~A~^, ~}" list))

(defun theme-fn (name)
  (find-symbol name (theme-package)))
