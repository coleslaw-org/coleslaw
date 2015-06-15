(defpackage #:coleslaw-cli
  (:use #:cl)
  (:documentation "CLI processing tools.")
  (:export
   #:process-parameters
   #:main))

(in-package #:coleslaw-cli)

(defun process-parameters (argv)
  "Return an alist where the key is the option name and the value the
  value. Only accept long form options."
  (unless (evenp (length argv))
    (error "All Options take a parameter."))
  (labels ((%iter (argv alist)
             (cond ((null argv) alist)
                   (t
                    (%iter (cddr argv)
                           (acons (alexandria:make-keyword
                                   (string-upcase
                                    (string-trim "-" (first argv))))
                                  (second argv)
                                  alist))))))
    (%iter argv nil)))

(defparameter *usage*
  "Usage: coleslaw <command> [--config <path-to-config>]

OPTIONS

    --config
      The coleslaw configuration file to use.


COMMANDS

  build
    build the site

  clean
    remove all the generated files.

  rebuild
    The equivalent of calling clean and build.

  serve
    Start a web server
")

(defun main (argv)
  "The CLI entry point."
  (format t "~A~%" *usage*))
