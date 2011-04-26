(eval-when (:compile-toplevel)
  (ql:quickload '(cxml split-sequence local-time cl-ppcre)))

(defpackage :coleslaw-import
  (:use :cl :coleslaw :cxml)
  (:import-from :local-time #:+short-month-names+
                            #:encode-timestamp)
  (:import-from :split-sequence #:split-sequence)
  (:import-from :cl-ppcre #:regex-replace-all))

(in-package :coleslaw-import)

(defgeneric import-post (service post &key static-p)
  (:documentation "Import POST into *storage*. The method to construct the POST
object is determined by SERVICE."))

(defmethod import-post ((service (eql :wordpress)) post &key static-p)
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
           (post-p ()
             (string= "post" (node-val "wp:post_type")))
           (make-timestamp (pubdate)
             (let* ((date (split-sequence #\Space (subseq pubdate 5)))
                    (time (split-sequence #\: (fourth date))))
               (encode-timestamp 0
                                 (parse-integer (third time))
                                 (parse-integer (second time))
                                 (parse-integer (first time))
                                 (parse-integer (first date))
                                 (position (second date) +short-month-names+
                                           :test #'string=)
                                 (parse-integer (third date))))))
    (when (and (public-p)
               (post-p))
      (let ((new-post (make-post (node-val "title")
                                 (node-val "category")
                                 (make-timestamp (node-val "pubDate"))
                                 (regex-replace-all (string #\Newline)
                                                    (node-val "content:encoded")
                                                    "<br>")
                                 :aliases (parse-integer (node-val "wp:post_id"))))
            (comments (nodes "wp:comment")))
        (add-post new-post (post-id new-post))
        (when static-p
          (ensure-directories-exist coleslaw::*input-dir*)
          (export-post new-post))))))

(defmethod export-post (post)
  (let ((filepath (merge-pathnames (format nil "~5,'0d-~a.post"
                                           (post-id post)
                                           (coleslaw::escape (post-title post)))
                                   coleslaw::*input-dir*)))
    (with-open-file (out filepath :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
      ;; TODO: What other data/metadata should we write out?
      (format out ";;;;;~%")
      (format out "title: ~A~%" (post-title post))
      (format out "tags: ~A~%" (coleslaw::pretty-list (post-tags post)))
      (format out "date: ~A~%" (coleslaw::year-month (post-date post)))
      (format out ";;;;;~%")
      (format out "~A~%" (post-content post)))))

(defgeneric import-posts (service filepath &key static-p)
  (:documentation "Import the posts (and potentially comments or other data)
from FILEPATH, converting them to appropriate coleslaw objects and inserting
them into *storage*. The method to parse the file is determined by SERVICE.
If STATIC-P is true, the posts will also be written into *.html files in
*input-dir*."))

(defmethod import-posts ((service (eql :wordpress)) filepath &key static-p)
  (let* ((xml (cxml:parse-file filepath (cxml-dom:make-dom-builder)))
         (posts (dom:get-elements-by-tag-name xml "item")))
    (loop for post across posts do
         (import-post service post :static-p static-p))))
