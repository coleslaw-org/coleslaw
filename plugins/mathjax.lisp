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
  MathJax.Hub.Config({
    tex2jax: {
      inlineMath: [['$$','$$']]
    }
  });
</script>
<script type=\"text/javascript\"
src=\"http://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML\">
</script>")

(defun enable ()
  (labels ((math-post-p (obj)
             (member "math" (content-tags obj) :test #'string=))
           (mathjax-p (obj)
             (etypecase obj
               (content (math-post-p obj))
               (index (some #'math-post-p (index-posts obj))))))
    (add-injection (list *mathjax-header* #'mathjax-p) :head)))
