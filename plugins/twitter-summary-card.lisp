(defpackage :coleslaw-twitter-summary-card
  (:use :cl :coleslaw)
  (:export #:enable))

(in-package :coleslaw-twitter-summary-card)

(defun summary-card (post twitter-handle)
  "TODO: Figure if and how to include twitter:url meta property."
  (format nil "<meta property=\"twitter:card\" content=\"summary\" />
~@[<meta property=\"twitter:author\" content=\"~A\" />~]
<meta property=\"twitter:title\" content=\"~A\" />
<meta property=\"twitter:description\" content=\"~A\" />"
          twitter-handle
          (title-of post)
          (let ((text (content-text post)))
            (if (< 200 (length text))
                (subseq text 0 199)
                text))))

(defun enable (&key twitter-handle)
  (add-injection
   (lambda (x)
     (when (typep x 'post)
       (summary-card x twitter-handle)))
                 :head))
