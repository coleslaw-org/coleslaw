(defpackage :coleslaw-matomo
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:add-injection))

(in-package :coleslaw-matomo)

(defvar *matomo-js*
  "<script type="text/javascript">
  var _paq = _paq || [];
  _paq.push(['trackPageView']);
  _paq.push(['enableLinkTracking']);

  (function() {
    var u='//~a/';
    _paq.push(['setTrackerUrl', u+'matomo.php']);
    _paq.push(['setSiteId', '~a']);
    var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
    g.type='text/javascript'; g.async=true; g.defer=true; g.src=u+'matomo.js'; s.parentNode.insertBefore(g,s);
  })();
</script>")

(defun enable (&key matomo-url matomo-site)
  (let ((snippet (format nil *matomo-js* matomo-url matomo-site)))
    (add-injection (constantly snippet) :head)))
