(defpackage #:coleslaw-cli
  (:use #:cl)
  (:import-from #:coleslaw-cli/build
                #:build)
  (:import-from #:coleslaw-cli/clean
                #:clean)
  (:import-from #:coleslaw-cli/rebuild
                #:rebuild)
  (:import-from #:coleslaw-cli/serve
                #:serve)
  (:import-from #:alexandria
                #:make-keyword)
  (:import-from #:command-line-arguments
                #:handle-command-line)
  (:export
   #:process-parameters
   #:main))

(in-package #:coleslaw-cli)

(setf (documentation *package* t)
      "Usage: coleslaw <command> [--config <path/to/config>] [--blog-dir <path/to/blog>] [--host <host>] [--port <port>]

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

(defun print-help ()
  (format t "~A~%" (documentation (find-package "COLESLAW-CLI") t)))

(defparameter +default-option-spec+
  '((("config" #\c) :type string :optional t :documentation "config file to use.")
    (("blog-dir" #\d) :type string :optional t :documentation "The directory where the blog is located"))
  "The option spec for every command of the CLI.")

(defparameter +serve-option-spec+
  (append +default-option-spec+
          '((("port" #\p) :type integer :optional t :documentation "the port to start the server on")
            (("host" #\h) :type string :optional t :documentation "the host to start the server on")))
  "The option spec for the serve command.")

(defun process-parameters (argv)
  "Returns the command name and the rest of the of the arguments."
  (if (null argv)
      (values (make-keyword "default") nil)
      (values (make-keyword (string-upcase (cadr argv))) (cddr argv))))


(defun main (argv)
  "The CLI entry point."
  (multiple-value-bind
        (command-name args) (process-parameters argv)
    (case command-name
      (:build
       (handle-command-line +default-option-spec+
                            #'build
                            :command-line args
                            :name "build"))
      (:clean
       (handle-command-line +default-option-spec+
                            #'clean
                            :command-line args
                            :name "clean"))
      (:rebuild
       (handle-command-line +default-option-spec+
                            #'rebuild
                            :command-line args
                            :name "rebuild"))
      (:serve
       (handle-command-line +serve-option-spec+
                            #'serve
                            :command-line args
                            :name "serve"))
      ((:default :help) (print-help))
      (otherwise (progn (format t "Unrecognized option ~A~%" command-name)
                        (print-help)))))
  (uiop:quit))
