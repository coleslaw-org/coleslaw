(defpackage :coleslaw-gtag
  (:use :cl)
  (:export #:enable)
  (:import-from :coleslaw #:add-injection))

(in-package :coleslaw-gtag)

(defvar *analytics-js*
"<!-- Global site tag (gtag.js) - Google Analytics -->
<script async src='https://www.googletagmanager.com/gtag/js?id=~a'></script>
<script>
  window.dataLayer = window.dataLayer || [];
  function gtag(){dataLayer.push(arguments);}
  gtag('js', new Date());

  gtag('config', '~a');
</script>")

(defun enable (&key tracking-code)
  (let ((snippet (format nil *analytics-js* tracking-code tracking-code)))
    (add-injection (constantly snippet) :head)))
