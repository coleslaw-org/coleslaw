(defpackage :coleslaw-mathjax
  (:use :cl)
  (:import-from :coleslaw #:add-injection
                          #:post
                          #:index
                          #:post-tags
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
  (labels ((math-post-p (post)
             (member "math" (post-tags post) :test #'string=))
           (mathjax-p (content)
             (etypecase content
               (post (math-post-p content))
               (index (some #'math-post-p (index-posts content))))))
    (add-injection (list *mathjax-header* #'mathjax-p) :head)))
