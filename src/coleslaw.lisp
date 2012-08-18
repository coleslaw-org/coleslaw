(in-package :coleslaw)

(defparameter *storage* nil
  "A db-spec for postmodern or a hash-table cache. It is expected that
*storage* has methods for each Generic Function in coleslaw implemented.")

(defgeneric get-credentials (name)
  (:documentation "Retrieve the credentials keyed by NAME from *storage*."))

(defgeneric set-credentials (name credentials)
  (:documentation "Store the given CREDENTIALS in *storage* under NAME."))

(defun load-config ()
  nil)

(defun exit-handler ()
  nil)

(defun compile-blog ()
  nil)

(defun main ()
  (load-config)
  (unwind-protect
       (loop do (if (blog-update-p)
                    (compile-blog)
                    (sleep 600)))
    (exit-handler)))
