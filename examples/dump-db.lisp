(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(coleslaw cl-store)))

(in-package :coleslaw)

(defun main ()
  (let ((db-file (rel-path (user-homedir-pathname) ".coleslaw.db")))
    (format t "~%~%Coleslaw loaded. Attempting to load config file.~%")
    (load-config "")
    (format t "~%Config loaded. Attempting to load blog content.~%")
    (load-content)
    (format t "~%Content loaded. Attempting to dump content database.~%")
    (cl-store:store *site* db-file)
    (format t "~%Content database saved to ~s!~%~%" (namestring db-file))))

(main)
(exit)
