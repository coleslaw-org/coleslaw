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
      (do-files (file (repo-dir *config*) file-type)
        (let ((obj (construct (class-name doc-type) (read-content file))))
          (add-document obj))))))

(defmethod discover :before (doc-type)
  (purge-all (class-name doc-type)))

;; Instance Methods

(defgeneric page-url (document)
  (:documentation "The relative URL to the DOCUMENT."))

(defgeneric render (document &key &allow-other-keys)
  (:documentation "Render the given DOCUMENT to HTML."))

;; Helper Functions

(defun compute-url (document unique-id &optional class)
  "Compute the relative URL for a DOCUMENT based on its UNIQUE-ID. If CLASS
is provided, it overrides the route used."
  (let* ((class-name (or class (class-name (class-of document))))
         (route (get-route class-name)))
    (unless route
      (error "No routing method found for: ~A" class-name))
    (let* ((result (if (typep route 'function)
                       (funcall route document)
                       (format nil route unique-id)))
           (type (or (pathname-type result) (page-ext *config*))))
      (make-pathname :type type :defaults result))))

(defun get-route (doc-type)
  "Return the route format string for DOC-TYPE."
  (second (assoc (make-keyword doc-type) (routing *config*))))

(defun add-document (document)
  "Add DOCUMENT to the in-memory database. Error if a matching entry is present."
  (let ((url (page-url document)))
    (if (gethash url *site*)
        (error "There is already an existing document with the url ~a" url)
        (setf (gethash url *site*) document))))

(defun delete-document (document)
  "Given a DOCUMENT, delete it from the in-memory database."
  (remhash (page-url document) *site*))

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
