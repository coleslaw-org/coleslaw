(defpackage :coleslaw
  (:use :cl :closure-template)
  (:export ;; coleslaw-core
           #:*storage*
           #:start-coleslaw
           #:stop-coleslaw
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

           ;; comments
           #:make-comment
           #:add-comment
           #:remove-comment
           #:render-comments
           #:find-comments

           #:author-name
           #:author-url
           #:author-ip

           #:comment-id
           #:comment-post
           #:comment-author
           #:comment-timestamp
           #:comment-content
           #:comment-parent

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
