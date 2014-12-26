#!/usr/bin/cl -Q -sp coleslaw --entry clean

;; clean files of deploy-dir and staging-dir

(defun clean (argv)
  (let* ((parameters (coleslaw-cli:process-parameters argv))
         (current-dir (uiop/os:getcwd))
         (repo-dir (or (cdr (assoc :repo-dir parameters))
                       current-dir))
         (config-file (or (cdr (assoc :config parameters))
                          (discover-config-path current-dir))))
    (load-config config-file repo-dir)
    (dolist (dir (list (staging-dir *config*)
                       (deploy-dir *config*)))
      (uiop/filesystem:delete-directory-tree (pathname dir)
                                             :if-does-not-exist :ignore
                                             :validate t))))
