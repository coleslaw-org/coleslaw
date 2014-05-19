(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'cl-store))

(defpackage :coleslaw-incremental
  (:use :cl)
  (:import-from :alexandria #:when-let)
  (:import-from :coleslaw #:all-subclasses
                          #:content
                          #:construct
                          #:get-updated-files
                          #:find-content-by-path
                          #:write-document
                          #:rel-path)
  (:export #:enable))

(in-package :coleslaw-incremental)

;; KLUDGE: We currently never update the site for config changes.
;; Examples to consider include changing the theme or domain of the site.
;; Both would require full site recompiles. Consequently, it seems reasonable
;; to expect that incremental plugin users:
;;   A) have done a full build of their site
;;   B) have a cl-store dump of the database at ~/.coleslaw.db
;;      ^ we should provide a script or plugin just for this
;;   C) move the original deployment to a location of their choice and
;;      set it as staging-dir in coleslaw's config prior to enabling incremental builds
;;   D) to further simplify *my* life, we assume the date of a piece of content will
;;      never be changed retroactively, only its tags

;; NOTE: We're gonna be a bit dirty here and monkey patch. The compilation model
;; still isn't an "exposed" part of Coleslaw. After some experimentation maybe
;; we'll settle on an interface.

(defvar *transients* '(coleslaw::numeric-index coleslaw::feed coleslaw::tag-feed)
  "A list of document types that should be regenerated on *any* change to the blog.")

(defun coleslaw::load-content ()
  (let ((db-file (rel-path (user-homedir-pathname) ".coleslaw.db")))
    (setf coleslaw::*site* (cl-store:restore db-file))
    (loop for (status path) in (get-updated-files)
       do (update-content status path))
    (coleslaw::update-content-metadata)
    (dolist (doc-type *transients*)
      (discover (find-class doc-type)))
    (cl-store:store coleslaw::*site* db-file)))

(defun update-content (status path)
  (cond ((string= "D" status) (process-change :deleted path))
        ((string= "M" status) (process-change :modified path))
        ((string= "A" status) (process-change :added path))))

(defgeneric process-change (status path &key &allow-other-keys)
  (:documentation "Updates the database as needed for the STATUS change to PATH.")
  (:method :around (status path &key)
    (let ((extension (pathname-type path))
          (ctypes (all-subclasses (find-class 'content))))
      ;; This feels way too clever. I wish I could think of a better option.
      (flet ((class-name-p (x class)
               (string-equal x (symbol-name (class-name class)))))
        ;; If the updated file's extension doesn't match one of our content types,
        ;; we don't need to mess with it at all. Otherwise, since the class is
        ;; annoyingly tricky to determine, pass it along.
        (when-let (ctype (find extension ctypes :test #'class-name-p))
          (call-next-method status path :ctype ctype))))))

;; TODO: We should check to see if a *new* tag or month exists
;; and create an index appropriately. If the last content from a
;; given month or with a given tag is deleted, just drop the index.
;; (And also remove it from *all-months* / *all-tags*. Should we store those?)
;; Additionally, the tag/month lists won't be updated on tag/month index pages.

(defmethod process-change ((status (eql :deleted)) path &key)
  (let ((old (find-content-by-path path)))
    ;; TODO: Remove from any tag and month indexes.
    (delete-document old)))

(defmethod process-change ((status (eql :modified)) path &key)
  (let ((old (find-content-by-path path))
        (new (construct ctype (read-content path))))
    (setf (gethash (page-url old) coleslaw::*site*) new)
    ;; TODO:
    ;; Iterate over tags in new, setting old to new in each tag index's content.
    ;; If there are new tags/date, add it to relevant indices.
    ;; If tags/date are removed, remove from relevant indices.
    ))

(defmethod process-change ((status (eql :added)) path &key ctype)
  (let ((new (construct ctype (read-content path))))
    ))

(defun delete-document (document)
  "Given a DOCUMENT, delete it from the staging directory and in-memory DB."
  (let ((url (page-url document)))
    (delete-file (rel-path (staging-dir *config*) (namestring url)))
    (remhash (page-url document) coleslaw::*site*)))

(defun coleslaw::compile-blog (staging)
  "lulz. Do it live. DO IT ALL LIVE."
  (dolist (doc-type *transients*)
    (publish (find-class doc-type)))
  ;; FIXME: This doesn't cover prev/next links for posts, theme-fn for feeds.
  (mapcar #'write-document *changed-content*))

;; No-op. We'll be updating in place instead.
(defmethod coleslaw:deploy (staging))

(defun enable ())
