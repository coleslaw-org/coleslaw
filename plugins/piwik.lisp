(defpackage :coleslaw-piwik
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:add-injection))

(in-package :coleslaw-piwik)

(defvar *piwik-js*
  "<script type="text/javascript">
  var _paq = _paq || [];
  _paq.push(['trackPageView']);
  _paq.push(['enableLinkTracking']);

  (function() {
    var u="//~a/";
    _paq.push(['setTrackerUrl', u+'piwik.php']);
    _paq.push(['setSiteId', {$IDSITE}]);
    var d=document, g=d.createElement('script'), s=d.getElementsByTagName('script')[0];
    g.type='text/javascript'; g.async=true; g.defer=true; g.src=u+'piwik.js'; s.parentNode.insertBefore(g,s);
  })();
</script>")

(defun enable (&key piwik-url)
  (let ((snippet (format nil *piwik-js* piwik-url)))
    (add-injection (constantly snippet) :head)))
