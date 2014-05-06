(in-package :coleslaw)

;;;; The Document Protocol

;; Data Storage

(defvar *site* (make-hash-table :test #'equal)
  "An in-memory database to hold all site documents, keyed on relative URLs.")

;; Class Methods

(defgeneric publish (doc-type)
  (:documentation "Write pages to disk for all documents of the given DOC-TYPE."))

(defgeneric discover (doc-type)
  (:documentation "Load all documents of the given DOC-TYPE into memory.")
  (:method (doc-type)
    (let ((file-type (format nil "~(~A~)" (class-name doc-type))))
      (do-files (file (repo *config*) file-type)
        (let ((obj (construct (class-name doc-type) (read-content file))))
          (add-document obj))))))

(defmethod discover :before (doc-type)
  (purge-all (class-name doc-type)))

;; Instance Methods

(defgeneric page-url (document)
  (:documentation "The url to the DOCUMENT without the domain.")
  (:method (document)
    (let* ((class-name (class-name (class-of document)))
           (route (assoc (make-keyword class-name) (routing *config*))))
      (if route
          (format nil (second route) (slot-value document 'slug))
          (error "No routing method found for: ~A" class-name)))))

(defmethod page-url :around ((document t))
  (let ((result (call-next-method)))
    (if (pathname-type result)
        result
        (make-pathname :type "html" :defaults result))))

(defgeneric render (document &key &allow-other-keys)
  (:documentation "Render the given DOCUMENT to HTML."))

;; Helper Functions

(defun add-document (document)
  "Add DOCUMENT to the in-memory database. Error if a matching entry is present."
  (let ((url (page-url document)))
    (if (gethash url *site*)
        (error "There is already an existing document with the url ~a" url)
        (setf (gethash url *site*) document))))

(defun write-document (document &optional theme-fn &rest render-args)
  "Write the given DOCUMENT to disk as HTML. If THEME-FN is present,
use it as the template passing any RENDER-ARGS."
  (let ((html (if (or theme-fn render-args)
                  (apply #'render-page document theme-fn render-args)
                  (render-page document nil)))
        (url (namestring (page-url document))))
    (write-file (rel-path (staging-dir *config*) url) html)))

(defun find-all (doc-type)
  "Return a list of all instances of a given DOC-TYPE."
  (loop for val being the hash-values in *site*
     when (typep val doc-type) collect val))

(defun purge-all (doc-type)
  "Remove all instances of DOC-TYPE from memory."
  (dolist (obj (find-all doc-type))
    (remhash (page-url obj) *site*)))
