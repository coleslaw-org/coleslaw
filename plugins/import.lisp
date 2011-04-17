(asdf:oos 'asdf:load-op 'cxml)
(asdf:oos 'asdf:load-op 'split-sequence)
(asdf:oos 'asdf:load-op 'local-time)

(defpackage :coleslaw-import
  (:use :cl :coleslaw :cxml)
  (:import-from :local-time #:+short-month-names+
                            #:encode-timestamp)
  (:import-from :split-sequence #:split-sequence))

(in-package :coleslaw-import)

(defgeneric import-post (service post)
  (:documentation "Import POST into *storage*. The method to construct the POST
object is determined by SERVICE."))

(defmethod import-post ((service (eql :wordpress)) post)
  (labels ((nodes (name)
             (dom:get-elements-by-tag-name post name))
           (value (node)
             (let ((child (dom:last-child node)))
               (when child (dom:data child))))
           (node-val (name)
             (let ((nodes (nodes name)))
               (if (string= "category" name)
                   (loop for node across nodes collecting (value node))
                   (when (plusp (length nodes)) (value (elt nodes 0))))))
           (public-p ()
             (string= "publish" (node-val "wp:status")))
           (make-timestamp (pubdate)
             (let* ((date (split-sequence #\Space (subseq pubdate 5)))
                    (time (split-sequence #\: (fourth date))))
               (encode-timestamp 0
                                 (parse-integer (third time)) ; sec
                                 (parse-integer (second time)) ; min
                                 (parse-integer (first time)) ; hr
                                 (parse-integer (first date)) ; day
                                 (position (second date) +short-month-names+
                                           :test #'string=) ; month
                                 (parse-integer (third date)))))) ; year
    (when (public-p)
      (let ((new-post (make-post (node-val "title")
                                 (format nil "~{~A~^, ~}" (node-val "category"))
                                 (make-timestamp (node-val "pubDate"))
                                 (node-val "content:encoded")
                                 :id (parse-integer (node-val "wp:post_id"))))
            (comments (nodes "wp:comment")))
        ;; TODO:
        ;; add comment handling ... (when comments ...)
        (add-post new-post (post-id new-post))))))

(defgeneric import-posts (service filepath)
  (:documentation "Import the posts (and potentially comments or other data)
from FILEPATH, converting them to appropriate coleslaw objects and inserting
them into *storage*. The method to parse the file is determined by SERVICE."))

(defmethod import-posts ((service (eql :wordpress)) filepath)
  (let* ((xml (cxml:parse-file filepath (cxml-dom:make-dom-builder)))
         (posts (dom:get-elements-by-tag-name xml "item")))
    (loop for post across posts do (import-post :wordpress post))))
