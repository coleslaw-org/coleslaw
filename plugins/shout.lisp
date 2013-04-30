(defpackage :coleslaw-shout
  (:use :cl)
  (:import-from :coleslaw #:*config*
                          #:content
                          ;; slots:
                          ;; content-slug, content-date, theme,
                          ;; helpers:
                          ;; slugify, by-date, and find-all.
                          ;; core methods?
                          ;; render, page-url, publish too?
                          #:render-content
                          #:theme-fn
                          #:page-path
                          #:render-page
                          #:write-page)
  (:export #:enable))

(in-package :coleslaw-shout)

#|
This plugin is pretty damn limited. There should be some sort of index
for shouts. Maybe we shouldn't even render them individually as they're
a bit boring. I also really wanted this to do some sort of smart
auto-embedding and cross-post to twitter. That certainly ups the
complexity of this "plugin" though. I should think more on this.
Additional content types are certainly possible without much difficulty.
It's much less awkward for them to be in the core though (e.g. imports).
And we really don't want theme authors having unexpected templates pop up.

If we are going to offer content types as plugins (and I'm leaning away)
we should make sure that they there is a sensible way to specify indices
for that content type.
|#

(defvar *template*
  "{namespace coleslaw.theme.~A}
{template shout}
<div class=\"article-meta\">{\n}
  <h1 class=\"title\">{$shout.target |noAutoescape|}</h1>{\n}
  <div class=\"via\">{\n}
    Found via {$shout.via |noAutoescape}
  </div>{\n}
  <div class=\"date\">{\n}
    Published on {$shout.date}
  </div>{\n}
</div>{\n}
<div class=\"article-content\">{\n}
  {$shout.text |noAutoescape}
<div>{\n}
{/template}")

(defclass shout (content)
  ((via :initform nil :initarg :via :accessor shout-via)
   (target :initform nil :initarg :target :accessor shout-target)))

(defmethod initialize-instance :after ((object shout) &key)
  (with-accessors ((via shout-via)
                   (target shout-target)) object
    (setf via (render-content via :md)
          target (render-content target :md)
          (content-slug object) (slugify (content-date object)))))

(defmethod render ((object shout))
  (funcall (theme-fn 'shout) (list :config *config* :shout object)))

(defmethod page-url ((object shout))
  (format nil "shouts/~d.html" (content-slug object)))

(defmethod publish ((content-type (eql :shout)))
  (dolist (shout (by-date (find-all 'shout)))
    (write-page (page-path shout) (render-page shout))))

(defun enable ()
  (let ((name (theme *config*)))
    (compile-template :common-lisp-backend (format nil *template* name))))
