(defpackage #:coleslaw-cli/rebuild
  (:use #:cl)
  (:documentation "Clean and Build the blog.")
  (:export
   #:rebuild))
(in-package #:coleslaw-cli/rebuild)

(defun rebuild (&key config blog-dir)
  (coleslaw-cli/clean:clean :config config :blog-dir blog-dir)
  (coleslaw-cli/build:build :config config :blog-dir blog-dir))

;; (defun rebuild (&key config)
;;   (let* ((parameters (coleslaw-cli:process-parameters argv))
;;          (current-dir (uiop/os:getcwd))
;;          (repo-dir (or (cdr (assoc :repo-dir parameters))
;;                        current-dir))
;;          (config-file (or (cdr (assoc :config parameters))
;;                           (discover-config-path current-dir)))
;;          (old-rev (inferior-shell:run/s
;;                    "git log --oneline -1 | awk -e '{print $1}'")))
;;     (load-config config-file repo-dir)
;;     (dolist (dir (list (staging-dir *config*)
;;                        (deploy-dir *config*)))
;;       (uiop/filesystem:delete-directory-tree (pathname dir)
;;                                              :if-does-not-exist :ignore
;;                                              :validate t))
;;     (main repo-dir old-rev)))
