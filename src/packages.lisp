(defpackage :coleslaw
  (:use :cl :closure-template)
  (:import-from :iolib.os #:with-current-directory
                          #:delete-files
                          #:read-symlink
                          #:run-program)
  (:export ;; themes
           #:add-injection
           #:remove-injection

           ;; plugins
           #:load-plugins

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
           #:index-posts))
