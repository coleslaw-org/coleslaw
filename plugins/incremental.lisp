(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 'cl-store))

(defpackage :coleslaw-incremental
  (:use :cl)
  (:import-from :alexandria #:when-let)
  (:import-from :coleslaw #:*config*
                          #:content
                          #:index
                          #:discover
                          #:get-updated-files
                          #:find-content-by-path
                          #:add-document
                          #:delete-document
                          ;; Private
                          #:all-subclasses
                          #:do-subclasses
                          #:read-content
                          #:construct
                          #:rel-path
                          #:repo
                          #:update-content-metadata)
  (:export #:enable))

(in-package :coleslaw-incremental)

;; In contrast to the original incremental plans, full of shoving state into
;; the right place by hand and avoiding writing pages to disk that hadn't
;; changed, the new plan is to only avoid redundant parsing of content in
;; the git repo. The rest of coleslaw's operation is "fast enough".
;;
;;   Prior to enabling the plugin a user must have a cl-store dump of the
;;   database at ~/.coleslaw.db. There is a dump_db shell script in
;;   examples to generate the database dump.
;;
;; We're gonna be a bit dirty here and monkey patch. The compilation model
;; still isn't an "exposed" part of Coleslaw. After some experimentation maybe
;; we'll settle on an interface.

(defun coleslaw::load-content ()
  (let ((db-file (rel-path (user-homedir-pathname) ".coleslaw.db")))
    (setf coleslaw::*site* (cl-store:restore db-file))
    (loop for (status path) in (get-updated-files)
       for file-path = (rel-path (repo-dir *config*) path)
       do (update-content status file-path))
    (update-content-metadata)
    ;; Discover's :before method will delete any possibly outdated indexes.
    (do-subclasses (itype index)
      (discover itype))
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

(defmethod process-change ((status (eql :deleted)) path &key)
  (let ((old (find-content-by-path path)))
    (delete-document old)))

(defmethod process-change ((status (eql :modified)) path &key ctype)
  (let ((old (find-content-by-path path))
        (new (construct ctype (read-content path))))
    (delete-document old)
    (add-document new)))

(defmethod process-change ((status (eql :added)) path &key ctype)
  (let ((new (construct ctype (read-content path))))
    (add-document new)))

(defun enable ())
