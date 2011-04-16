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

           ;; WARNING: STILL IN FLUX
           ;; posts
           #:make-post
           #:add-post
           #:remove-post
           #:render-post
           #:find-post
           #:find-by-tag
           #:find-by-date
           #:find-by-range

           ;; comments
           #:make-comment
           #:add-comment
           #:remove-comment
           #:render-comments
           #:find-comments

           ;; indices
           #:add-index
           #:remove-index
           #:render-index
           #:find-index
           ))
