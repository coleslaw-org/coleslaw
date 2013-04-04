(defpackage :coleslaw-mathjax
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:add-injection
                          #:content
                          #:index
                          #:content-tags
                          #:index-posts))

(in-package :coleslaw-mathjax)

(defvar *mathjax-header* "<script type=\"text/x-mathjax-config\">
  MathJax.Hub.Config({~@[~A~]});
</script>
<script type=\"text/javascript\" src=\"~A~@[?config=~A~]\"></script>")

(defun enable (&key force config (preset "TeX-AMS-MML_HTMLorMML")
                 (location "http://cdn.mathjax.org/mathjax/latest/MathJax.js"))
  (labels ((math-post-p (obj)
             (member "math" (content-tags obj) :test #'string=))
           (mathjax-p (obj)
             (or force
                 (etypecase obj
                   (content (math-post-p obj))
                   (index (some #'math-post-p (index-posts obj)))))))
    (let ((mathjax-header (format nil *mathjax-header* config location preset)))
      (add-injection (list mathjax-header #'mathjax-p) :head))))
