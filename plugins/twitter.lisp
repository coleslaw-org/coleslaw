(:eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload :chirp))

(defpackage :coleslaw-twitter
  (:use :cl)
  (:import-from :coleslaw
                :*config*
                :deploy
                :get-updated-files
                :page-url
                :plugin-conf-error)
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
             :message "Credentials missing."))

  ;; fallback to chirp for credential erros
  (chirp:account/verify-credentials)
  
  (when tweet-format
    (setf *tweet-format* tweet-format)))


(defmethod deploy :after (staging)
  (declare (ignore staging))
  (loop :for (state file) :in (get-updated-files)
     :when (and (string= "A" state) (string= "post" (pathname-type file)))
     :do (tweet-new-post file)))

(defun tweet-new-post (file)
  "Retrieve most recent post from in memory DB and publish it."
  (let ((post (coleslaw::find-content-by-path file)))
    (chirp:statuses/update (%format-post 0 post))))

(defun %format-post (offset post)
  "Guarantee that the tweet content is 140 chars at most. The 117 comes from
the spaxe needed for a space and the url."
  (let* ((content-prefix (subseq (render-tweet post) 0 (- 117 offset)))
         (content (format nil "~A ~A/~A" content-prefix
                          (coleslaw::domain *config*)
                          (page-url post)))
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
