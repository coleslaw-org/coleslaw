(defpackage #:coleslaw-cli/build
  (:use #:cl)
  (:documentation "Build the blog.")
  (:export
   #:build))
(in-package #:coleslaw-cli/build)

(defun build (&key config blog-dir)
  (let
      ((old-rev (inferior-shell:run/s
                 "git log --oneline -1 | awk -e '{print $1}'"))
       (blog-dir (uiop/os:getcwd)))
    (coleslaw:main blog-dir old-rev config)))

(setf (documentation #'build 'function)
      (documentation *package* t))
