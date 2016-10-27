(defpackage :coleslaw-isso
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:add-injection
                          #:post))

(in-package :coleslaw-isso)

(defvar *isso-header*
  "<div class=\"comments\">
      <section id=\"isso-thread\"></section>
      <script data-isso=\"~a/\"
              src=\"~a/js/embed.min.js\"></script>
    </div>")

(defun enable (&key isso-url)
  (flet ((inject-p (x)
           (when (typep x 'post)
             (format nil *isso-header* isso-url isso-url))))
    (add-injection #'inject-p :body)))
