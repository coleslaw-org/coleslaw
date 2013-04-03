(defpackage :coleslaw-mathjax
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:add-injection
                          #:content
                          #:index
                          #:content-tags
                          #:index-posts))

(in-package :coleslaw-mathjax)

(defvar *mathjax-config-header* "<script type=\"text/x-mathjax-config\">
  MathJax.Hub.Config({
    ~A
  });
</script>
")

(defvar *default-mathjax-config* "tex2jax: {inlineMath: [['$$','$$']]}")

(defvar *mathjax-load-header-no-config* "<script type=\"text/javascript\"
src=\"~A\"> 
</script>
")

(defvar *mathjax-load-header-with-config* "<script type=\"text/javascript\"
src=\"~A?config=~A\"> 
</script>
")

(defun enable (&key force 
		 (mathjax-url "http://cdn.mathjax.org/mathjax/latest/MathJax.js")
		 (config "TeX-AMS-MML_HTMLorMML")
		 (mathjax-config *default-mathjax-config*))
  (labels ((math-post-p (obj)
             (member "math" (content-tags obj) :test #'string=))
           (mathjax-p (obj)
	     (or force
		 (etypecase obj
		   (content (math-post-p obj))
		   (index (some #'math-post-p (index-posts obj)))))))

    (let ((mathjax-header 
	   (concatenate 'string
			(if mathjax-config (format nil *mathjax-config-header* mathjax-config) "")
			(if config
			    (format nil *mathjax-load-header-with-config* mathjax-url config)
			    (format nil *mathjax-load-header-no-config* mathjax-url)))))
      (add-injection (list mathjax-header #'mathjax-p) :head))))
