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

;; FIXME: We currently never update the site for config changes.
;; Examples to consider include changing the theme or domain of the site.
;; Both would require full site recompiles. Consequently, it seems reasonable
;; to expect that incremental plugin users:
;;   A) have done a full build of their site
;;   B) have a cl-store dump of the database at ~/.coleslaw.db
;;      ^ we should provide a script or plugin just for this

;; NOTE: We're gonna be a bit dirty here and monkey patch. The compilation model
;; still isn't an "exposed" part of Coleslaw. After some experimentation maybe
;; we'll settle on an interface.

(defvar *changed-content* nil
  "A list of changed content instances to iterate over and write out to disk.")

(defun coleslaw::load-content ()
  (let ((db-file (rel-path (user-homedir-pathname) ".coleslaw.db")))
    (setf coleslaw::*site* (cl-store:restore db-file))
    (loop for (status path) in (get-updated-files)
       do (update-content status path))
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
        (when-let (ctype (find extension ctypes :test #'class-name-p))
          (call-next-method status path :ctype ctype))))))

(defmethod process-change ((status (eql :deleted)) path &key)
  (let ((obj (find-content-by-path path)))
    ))

(defmethod process-change ((status (eql :modified)) path &key)
  (let ((obj (find-content-by-path path)))
    ))

(defmethod process-change ((status (eql :added)) path &key ctype)
  (let ((obj (construct ctype (read-content path))))
    ))

(defun coleslaw::compile-blog (staging)
  "lulz. Do it live. DO IT ALL LIVE."
  ;; FIXME: This doesn't cover prev/next links for posts, theme-fn for feeds.
  (mapcar #'write-document *changed-content*))

;; No-op. We'll be updating in place instead.
(defmethod coleslaw:deploy (staging))

(defun enable ())
