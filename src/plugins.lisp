(in-package :coleslaw)

(defun load-plugins (plugins)
  "Resolve the path of each symbol in PLUGINS and call LOAD on the
resulting pathnames. It is expected that the matching *.lisp files
are in the plugins folder in coleslaw's source directory."
  (let ((files (mapcar (lambda (sym)
                         (merge-pathnames
                          (concatenate 'string "plugins/"
                                       (string-downcase (symbol-name sym)))
                          (asdf:system-source-directory 'coleslaw)))
                       plugins)))
    (map nil (lambda (file)
               (compile-file file)
               (load file)) files)))
