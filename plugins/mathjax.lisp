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

(defun enable (&key force config (preset "TeX-AMS-MML_HTMLorMML")
                 (location "http://cdn.mathjax.org/mathjax/latest/MathJax.js"))
  (labels ((math-post-p (obj)
             ;; Would it be better to test against latex than math, here?
             (tag-p "math" obj))
           (mathjax-p (obj)
             (or force
                 (etypecase obj
                   (content (math-post-p obj))
                   (index (some #'math-post-p (index-content obj)))))))
    (let ((mathjax-header (format nil *mathjax-header* config location preset)))
      (add-injection (list mathjax-header #'mathjax-p) :head))))
