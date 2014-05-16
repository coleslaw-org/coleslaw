(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'cl-store))

(defpackage :coleslaw-incremental
  (:use :cl)
  (:import-from :coleslaw #:get-updated-files
                          #:find-content-by-path
                          #:write-document)
  (:export #:enable))

(in-package :coleslaw-incremental)

;; FIXME: We currently never update the site for config changes.
;; Examples to consider include changing the theme or domain of the site.

;; NOTE: We're gonna be a bit dirty here and monkey patch. The compilation model
;; still isn't an "exposed" part of Coleslaw. After some experimentation maybe
;; we'll settle on an interface.

(defvar *changed-content* nil
  "A list of changed content instances to iterate over and write out to disk.")

(defun coleslaw::load-content ()
  ;; TODO: What if the file doesn't exist?
  (let ((db-file (rel-path (user-homedir-pathname) ".coleslaw.db")))
    (setf coleslaw::*site* (cl-store:restore db-file))
    (loop for (status path) in (get-updated-files)
       do (update-content status path))
    (cl-store:store coleslaw::*site* db-file)))

(defun update-content (status path)
  (cond ((string= "D" status) (process-change :deleted path))
        ((string= "M" status) (process-change :modified path))
        ((string= "A" status) (process-change :added path))))

(defgeneric process-change (status path)
  (:documentation "Updates the database as needed for the STATUS change to PATH."))

(defun coleslaw::compile-blog (staging)
  "lulz. Do it live. DO IT ALL LIVE."
  ;; FIXME: This doesn't cover prev/next links for posts, theme-fn for feeds.
  (mapcar #'write-document *changed-content*))

;; No-op. We'll be updating in place instead.
(defmethod coleslaw:deploy (staging))

(defun enable ())
