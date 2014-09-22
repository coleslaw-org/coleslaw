(defpackage :coleslaw-analytics
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:add-injection))

(in-package :coleslaw-analytics)

(defvar *analytics-js*
  "<script type=\"text/javascript\">

  var _gaq = _gaq || [];
  _gaq.push(['_setAccount', '~a']);
  _gaq.push(['_trackPageview']);

  (function() {
    var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;
    ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';
    var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);
  })();

</script>")

(defun enable (&key tracking-code)
  (let ((snippet (format nil *analytics-js* tracking-code)))
    (add-injection (constantly snippet) :head)))
