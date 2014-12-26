#!/usr/bin/cl -Q -sp coleslaw --entry rebuild

(defun rebuild (argv)
  (let* ((parameters (coleslaw-cli:process-parameters argv))
         (current-dir (uiop/os:getcwd))
         (repo-dir (or (cdr (assoc :repo-dir parameters))
                       current-dir))
         (config-file (or (cdr (assoc :config parameters))
                          (discover-config-path current-dir)))
         (old-rev (inferior-shell:run/s
                   "git log --oneline -1 | awk -e '{print $1}'")))
    (load-config config-file repo-dir)
    (dolist (dir (list (staging-dir *config*)
                       (deploy-dir *config*)))
      (uiop/filesystem:delete-directory-tree (pathname dir)
                                             :if-does-not-exist :ignore
                                             :validate t))
    (main repo-dir old-rev)))
