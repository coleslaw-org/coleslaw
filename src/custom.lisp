(in-package :coleslaw)
(defparameter *site-folder-local* "~/truexeu/")
(defparameter *site-folder-remote* "/srv/http/")

(defun rootstatic* (from to)
  (asdf::run-program (format nil "rsync -r --rsh=\"/usr/bin/sshpass -f /home/jose/.backup-pass ssh -o StrictHostKeyChecking=no\" ~A~A root@spensertruex.com:~A~A" *site-folder-local* from *site-folder-remote* to)))
(defcollect rootstatics* rootstatic* 2)
(defun truex ()
  (main *site-folder-local*)
  (rootstatics* "static" "static"
                "/static/google71c0326c2809a4a5.html" "google71c0326c2809a4a5.html"
                "/static/sitemap-2019-06-06.xml" "sitemap.xml"))
#|(in-package :coleslaw-cl-who)
(defun pack (el list?)
(if (and (listp list?) (listp (car list?)))
(cons el list?)
(list el list?)))
(defmacro lst (orderliness &rest elements)
``(,',orderliness ,@(mapcar (lambda (x) (pack :li x)) ',elements)))|#

