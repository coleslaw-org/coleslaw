(in-package :coleslaw)
(defparameter *site-folder-local* "~/truexeu/")
(defparameter *site-folder-remote* "/srv/http/")
(defun git-command (args)
  "Automatically git commit and push the blog to remote."
  (run-lines *site-folder-local*
             "git ~A"))
(defun git-stage ()
  (git-command "stage -A"))
(defun git-commit (&optional (commit-message "Automatic commit."))
  (git-command (format nil "commit '~A'" commit-message)))
(defun git-push ()
  (git-command "push"))
(defun rootstatic* (from to)
  (asdf::run-program (format nil "rsync -r --rsh=\"/usr/bin/sshpass -f /home/jose/.backup-pass ssh -o StrictHostKeyChecking=no\" ~A~A root@spensertruex.com:~A~A" *site-folder-local* from *site-folder-remote* to)))
(defcollect rootstatics* rootstatic* 2)
(defun truex ()
  (let ((backup-folder "~/site-backups/"))
    (main *site-folder-local*)
    (rootstatics* "static" "static"
                  "/static/google71c0326c2809a4a5.html" "google71c0326c2809a4a5.html"
                  "/static/sitemap-2019-06-06.xml" "sitemap.xml")
    (cond (hard-versioning (asdf::run-program (format nil "tar -cjf ~Aarchive-~S.tar.bz2 ~A" backup-folder (get-universal-time) *site-folder-local*)))
          (git-versioning (git-commit)
                          (git push))
          (versioned nil))))
#|(in-package :coleslaw-cl-who)
(defun pack (el list?)
(if (and (listp list?) (listp (car list?)))
(cons el list?)
(list el list?)))
(defmacro lst (orderliness &rest elements)
``(,',orderliness ,@(mapcar (lambda (x) (pack :li x)) ',elements)))|#

