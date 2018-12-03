(in-package :coleslaw)
(defun truex ()
  (let ((site-folder-local "~/truexeu/")
	(backup-folder "~/site-backups/"))
    (generate-irc-page site-folder-local)
    (let ((site-folder-remote site-folder-local))
      (main site-folder-local)
      (asdf::run-program (format nil "rsync -r ~Astatic ~Astatic" site-folder-local site-folder-remote))
      (asdf::run-program (format nil "tar -cjf ~Aarchive-~S.tar.bz2 ~A" backup-folder (get-universal-time) site-folder-local)))))
(defmacro fn-over-file ((var file) &rest body)
  `(with-open-file (,var ,file :direction :output
		       :if-exists :supersede
		       :if-does-not-exist :create)
     ,@body))
(defun read-lines (str)
  (with-input-from-string (s str)
    (labels ((line () (let ((l (read-line s nil nil)))
			(when l (read-from-string (concatenate 'string "\"" l "\"") nil nil)))))
      (let ((ls))
	(loop (let ((l (line)))
		(if (null l)
		    (return (nreverse ls))
		    (push l ls))))))))
(defun bounced (user domain config-dir)
  "What does ZNC bounce?"
  ;; Regex might break if a channel uses a name I didn't account for. ##name and #name-name work though. 
  (read-lines (asdf::run-program (format nil "ssh ~A\@~A 'cat ~Aznc.conf | grep -io \\#[\\#a-z0-9\\-]*'" user domain config-dir) :output :string)))
(defun generate-irc-page (folder)
  (let ((chans (bounced "jose" "truex.eu" "~/.znc/configs/")))
    (fn-over-file (s (merge-pathnames #P"irc-chan" folder)) (prin1 chans s))
    (fn-over-file (s (merge-pathnames #P"irc-list.page" folder))
		  (format s
			  (concatenate 'string
				       ";;;;;
title: IRC Bouncer hosted at this site
url: irc.html
format: cl-who
;;;;;

"
				       (write-to-string `(htm (:h1 "To request an account for the bouncer, send me an " (:a :href "mailto:spensertruexonline@gmail.com" "email"))
							      (:h2 "List of Irc Channels Bounced on This Site")
							      "<!--more-->"
							      (:ul ,@(loop for x in chans
									collect `(:li (esc ,x))))
							      "<!--more-->"
							      )))))))
