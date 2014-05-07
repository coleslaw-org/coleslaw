(:eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :chirp))

(defpackage :coleslaw-twitter
  (:use :cl)
  (:import-from :coleslaw
                :*config*
                :publish)
  (:export #:enable))

(in-package :coleslaw-twitter)

(defvar *tweet-format* '("~A by ~A" coleslaw::post-title coleslaw::post-author)
  "Controls what the tweet annoucing the post looks like. It contains a format
  control string followed with the accesors to evaluate for post.")

(defun enable (&key api-key api-secret access-token access-secret tweet-format)
  (if (and api-key api-secret access-token access-secret)
      (setf chirp:*oauth-api-key* api-key
            chirp:*oauth-api-secret* api-secret
            chirp:*oauth-access-token* access-token
            chirp:*oauth-access-secret* access-secret)
      (error 'plugin-conf-error :plugin "twitter"
             :message "Credentials missing.")
      ;; fallback to chirp for credential erros
      (chirp:account/verify-credentials))
  
  (when tweet-format
    (setf *tweet-format* tweet-format)))

(defmethod publish :after (post (eql (find-class 'coleslaw:post)))
  (format-post post))

(defun format-post (post)
  "Take a post and return a string of 140 character length, at most. Urls have 23 len and are a must."
 (chirp:statuses/update (%format-post post)))

(defun %format-post (offset post)
  "Garauntee that the tweet content is 140 chars at most."
  (let* ((content-prefix (subseq (render-tweet post) 0 (- 117 offset)))
         (content (format nil "~A ~A/~A" content-prefix
                          (coleslaw::domain *config*)
                          (coleslaw:page-url post)))
         (content-length (chirp:compute-status-length content)))
    (cond
      ((>= 140 content-length) content)
      ((< 140 content-length) (%format-post (1- offset) post)))))

(defun render-tweet (post)
  "Sans the url, which is a must."
  (apply #'format `(nil ,(car *tweet-format*)
                        ,@(loop
                             :for accesor in (cdr *tweet-format*)
                             :collect (funcall accesor post)))))
