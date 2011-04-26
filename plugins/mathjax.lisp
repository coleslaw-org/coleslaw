(defpackage :coleslaw-mathjax
  (:use :cl :coleslaw))

(in-package :coleslaw-mathjax)

(defvar *mathjax-header* "<script type=\"text/x-mathjax-config\">
  MathJax.Hub.Config({tex2jax: {inlineMath: [['\\(','\\)']]}});
</script>
<script type=\"text/javascript\"
src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\">
</script>")

(defmethod enable ()
  (coleslaw::add-injection *mathjax-header* :head))

(defmethod disable ()
  (coleslaw::remove-injection *mathjax-header* :head))
