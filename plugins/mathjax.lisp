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
                 (location "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js"))
  (flet ((inject-p (x)
           (when (or force (mathjax-p x))
             (format nil *mathjax-header* config location preset))))
    (add-injection #'inject-p :head)))
