(in-package :coleslaw)
(defparameter *site-folder-local* "~/truexeu/")
(defparameter *site-folder-remote* "/srv/http/")
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun group (source n)
    (labels ((rec (source acc)
               (let ((rest (nthcdr n source)))
                 (if (consp rest)
                     (rec rest (cons
                                (subseq source 0 n)
                                acc))
                     (nreverse
                      (cons source acc))))))
      (if source (rec source nil) nil))))
(defmacro with-gensyms (symbols &body body)
  "Create gensyms for those symbols."
  `(let (,@(mapcar #'(lambda (sym)
		       `(,sym ',(gensym))) symbols))
     ,@body))
(defmacro defcollect (name collector argn)
  "Collect a bunch of args into multiple invocations of a funcion. One example
of this is setf/setq: (setf a b c d) -> (setf a b) (setf c d)"
  (with-gensyms (x args)
    `(defmacro ,name (&rest ,args)
       `(progn ,@(loop for ,x in (group ,args ,argn)
                       collect (cons ',collector ,x))))))
(defun rootstatic* (from to)
  (asdf::run-program (format nil "rsync -r --rsh=\"/usr/bin/sshpass -f /home/jose/.backup-pass ssh -o StrictHostKeyChecking=no\" ~A~A root@spensertruex.com:~A~A" *site-folder-local* from *site-folder-remote* to)))
(defcollect rootstatics* rootstatic* 2)
(defun truex ()
  (let ((backup-folder "~/site-backups/"))
    (main *site-folder-local*)
    (rootstatics* "static" "static"
                  "/static/google71c0326c2809a4a5.html" "google71c0326c2809a4a5.html"
                  "/static/sitemap.xml" "sitemap.xml"
                  "/static/.htaccess" ".htaccess")
    (asdf::run-program (format nil "tar -cjf ~Aarchive-~S.tar.bz2 ~A" backup-folder (get-universal-time) *site-folder-local*))))
