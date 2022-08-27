(defpackage :coleslaw-disqus
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:add-injection
                          #:post))

(in-package :coleslaw-disqus)

(defparameter *disqus-header*
  "<div id=\"disqus_thread\"></div>
  <script>
  var disqus_config = function () {
  this.page.url = \"~A\";
  this.page.identifier = \"~A\";
  };
  (function() { // DON'T EDIT BELOW THIS LINE
           var d = document, s = d.createElement('script');
           s.src = 'https://~A.disqus.com/embed.js';
           s.setAttribute('data-timestamp', +new Date());
           (d.head || d.body).appendChild(s);
           })();
           </script>
           <noscript>Please enable JavaScript to view the <a href=\"https://disqus.com/?ref_noscript\">comments powered by Disqus.</a></noscript>")

(defun enable (&key shortname site-url)
  (flet ((inject-p (x)
           (when (typep x 'post)
             (format nil *disqus-header* site-url shortname shortname))))
    (add-injection #'inject-p :body)))

