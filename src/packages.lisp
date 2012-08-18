(defpackage :coleslaw
  (:use :cl :closure-template)
  (:import-from :iolib.os #:with-current-directory
                          #:*temporary-directory*)
  (:export ;; coleslaw-core
           #:*storage*
           #:get-credentials
           #:set-credentials

           ;; themes
           #:*current-theme*
           #:*theme-dir*
           #:add-injection
           #:remove-injection

           ;; posts
           #:make-post
           #:add-post
           #:remove-post
           #:render-post
           #:find-post
           #:post-url

           #:post-id
           #:post-title
           #:post-tags
           #:post-date
           #:post-content
           #:post-aliases

           ;; indices
           #:make-index
           #:add-to-index
           #:remove-from-index
           #:render-index
           #:find-index
           #:index-url

           #:index-id
           #:index-posts

           ;; plugins
           #:load-plugins))
