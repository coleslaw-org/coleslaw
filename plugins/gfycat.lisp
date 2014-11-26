(defpackage :coleslaw-gfycat
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:add-injection
                          #:content
                          #:tag-p))

(in-package :coleslaw-gfycat)

(defvar *gfycat-header*
  "<script>
 (function(d, t) {
    var g = d.createElement(t),
        s = d.getElementsByTagName(t)[0];
    g.src = 'http://assets.gfycat.com/js/gfyajax-0.517d.js';
    s.parentNode.insertBefore(g, s);
}(document, 'script'));
</script>")

(defun enable ()
  (flet ((inject-p (x)
           (when (and (typep x 'content)
                      (tag-p "gfycat" x))
             *gfycat-header*)))
    (add-injection #'inject-p :head)))
