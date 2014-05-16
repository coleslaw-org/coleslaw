(defpackage :coleslaw-mathjax
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:add-injection
                          #:content
                          #:index
                          #:tag-p
                          #:index-content))

(in-package :coleslaw-mathjax)

(defvar *mathjax-header* "~@[<script type=\"text/x-mathjax-config\">
  MathJax.Hub.Config({~A});
</script>~]
<script type=\"text/javascript\" src=\"~A~@[?config=~A~]\"></script>")

(defgeneric mathjax-p (document)
  (:documentation "Test if DOCUMENT requires contains any math-tagged content.")
  (:method ((content content))
    (tag-p "math" content))
  (:method ((index index))
    (and (slot-boundp index 'content)
         (some #'mathjax-p (index-content index)))))

(defun enable (&key force config (preset "TeX-AMS-MML_HTMLorMML")
                 (location "http://cdn.mathjax.org/mathjax/latest/MathJax.js"))
  (flet ((plugin-p (x) (or force (mathjax-p x))))
    (let ((mathjax-header (format nil *mathjax-header* config location preset)))
      (add-injection (list mathjax-header #'plugin-p) :head))))
