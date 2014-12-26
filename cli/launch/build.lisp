#!/usr/bin/cl -Q -sp coleslaw --entry  build
;;;;
;; This script assumes it is executed from the repository's top level directory
;; to determine correctly the blog-dir variable.
;;;;

(defun build (argv)
  (declare (ignorable argv))
  (let
      ((old-rev (inferior-shell:run/s
                 "git log --oneline -1 | awk -e '{print $1}'"))
       (blog-dir (uiop/os:getcwd)))
    (main blog-dir old-rev)))
