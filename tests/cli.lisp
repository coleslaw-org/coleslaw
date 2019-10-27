
(in-package :coleslaw-tests)

(plan 2)

(let ((*default-pathname-defaults*
       (pathname
        (format nil "~a/"
                (uiop:run-program `("mktemp" "-d")
                                  :output `(:string :stripped t))))))
  (coleslaw-cli:setup)
  (let ((file (coleslaw-cli:new)))
    (ok (probe-file file)))
  (coleslaw-cli:deploy)
  (print (format nil "~adeploy/index.html" *default-pathname-defaults*))
  (ok (probe-file (format nil "~adeploy/index.html" *default-pathname-defaults*))))

(finalize)

  
  

  
