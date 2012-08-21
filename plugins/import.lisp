(eval-when (:load-toplevel)
  (ql:quickload '(coleslaw cxml cl-ppcre local-time)))

(defpackage :coleslaw-import
  (:use :cl :cxml)
  (:import-from :coleslaw #:slugify
                          #:load-config
                          #:*config*
                          #:repo)
  (:import-from :local-time #:+short-month-names+
                            #:encode-timestamp)
  (:import-from :cl-ppcre #:regex-replace-all))

(in-package :coleslaw-import)

(defun import-post (post)
  (labels ((nodes (name)
             (dom:get-elements-by-tag-name post name))
           (value (node)
             (let ((child (dom:last-child node)))
               (when child (dom:data child))))
           (node-val (name)
             (let ((nodes (nodes name)))
               (if (string= "category" name)
                   (loop for node across nodes collecting (value node))
                   (when (plusp (length nodes)) (value (elt nodes 0)))))))
    (when (and (string= "publish" (node-val "wp:status")) ; is it public?
               (string= "post" (node-val "wp:post_type"))) ; is it a post?
      (export-post (node-val "title") (node-val "category") (node-val "pubDate")
                   (regex-replace-all (string #\Newline)
                                      (node-val "content:encoded") "<br>")
                   (format nil "~a.post" (slugify (node-val "title")))))))

(defun export-post (title tags date content path)
  (with-open-file (out (merge-pathnames path (repo *config*))
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create)
    ;; TODO: What other data/metadata should we write out?
    (format out ";;;;;~%")
    (format out "title: ~A~%" title)
    (format out "tags: ~A~%" (format nil "~{~A, ~}" tags))
    (format out "date: ~A~%" date)
    (format out "format: html~%") ; post format: html, md, rst, etc
    (format out ";;;;;~%")
    (format out "~A~%" (post-content post))))

(defun import-posts (filepath)
  (let* ((xml (cxml:parse-file filepath (cxml-dom:make-dom-builder)))
         (posts (dom:get-elements-by-tag-name xml "item")))
    (load-config)
    (ensure-directories-exist (repo *config*))
    (dolist (post posts)
      (import-post post))))
