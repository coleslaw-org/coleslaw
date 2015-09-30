(in-package :coleslaw)

;; Ctype Registry

;; (defvar *ctype-registry* '((post :indexable t
;;                                  :superclasses nil)))
(defvar *ctype-registry* nil
  "Registry of CTYPE records. Each record is a LIST. The CAR of each LIST is the CTYPE name. The CDR is a PLIST of CTYPE attributes.")

(defun ensure-ctype-record (ctype
                              &key
                                (indexable nil)
                                (superclasses nil))
  "Ensure a specific CTYPE record exists in *CTYPE-REGISTRY*. Do not change record if exists. Add record with specified parameters if absent."
  (unless (assoc ctype *ctype-registry*)
      (pushnew (list ctype
                   :indexable indexable
                   :superclasses superclasses)
             *ctype-registry*
             :key #'car)))

(defun get-ctype-attributes (ctype)
  "Get the CDR of the record which is either a PLIST of attributes or NIL if CTYPE record doesn't exist."
  (cdr (assoc ctype *ctype-registry*)))

(defun ctype-indexable-p (ctype)
  "Return T if CTYPE is indexable."
  (getf (get-ctype-attributes ctype) :indexable))

(defun set-ctype-indexable (ctype indexable-p)
  "Set the value of CTYPE's :INDEXABLE attribute to INDEXABLE-P. INDEXABLE-P is either T or NIL."
  (let ((ctype-register (get-ctype-attributes ctype)))
    (setf (getf ctype-register :indexable) indexable-p)))

(defun indexable-ctypes ()
  "Loop over *CTYPE-REGISTRY* and return a LIST of INDEXABLE CTYPES. Used to gather content for index generation."
  (loop for ctype in *ctype-registry*
        when (ctype-indexable-p (car ctype))
          collect (car ctype)))

(defun register-ctype-superclass (ctype superclass)
  "Register the SUPERCLASS as a direct superclass of the content type CTYPE."
  (ensure-ctype-record ctype)
  (let ((ctype-register (get-ctype-attributes ctype)))
    (or (member superclass (getf ctype-register :superclasses))
        (setf (getf ctype-register :superclasses)
              (append (getf ctype-register :superclasses)
                      (list superclass))))))

(defun register-superclass-to-ctypes (superclass &rest ctypes)
  "Register the SUPERCLASS to all specified CTYPES."
  (mapcar #'(lambda (ctype)
              (register-ctype-superclass ctype superclass))
          ctypes))

(defun ensure-ctype (ctype)
  "Ensure CTYPE class exists. Define the class if it is undefined. Update the list of direct superclasses of CTYPE class according to the corresponding CTYPE record in *CTYPE-REGISTRY*."
  (let* ((superclasses (cons 'content
                             (getf (get-ctype-attributes ctype)
                                   :superclasses))))
    (closer-mop:ensure-class ctype
                             :direct-superclasses
                             superclasses)))

(defun ensure-ctypes (ctypes)
  "Apply ENSURE-CTYPE to all specified CTYPES."
  (mapcar #'ensure-ctype ctypes))

(defmacro defcontent (name indexable-p &rest body)
  "Define a CTYPE class sequentially subclassing the list of superclasses defined in CTYPE record in *CTYPE-REGISTRY*. Define if the content type will be indexable or not."
  (let* ((ctype-register (get-ctype-attributes name))
         (superclasses (getf ctype-register :superclasses)))
    `(progn (or (ensure-ctype-record ',name :indexable ,indexable-p)
                (set-ctype-indexable ',name ,indexable-p))
            (defclass ,name ,(append '(content) superclasses) ,@body))))

(defmacro defmodifier (name
                       ctypes
                       direct-superclasses
                       direct-slots
                       &rest options)
  "Define a CTYPE MODIFIER. A MODIFIER class modifies and extends the behaviour of a content class by directly superclassing it. Its definition is exactly like a class definition except the list of CTYPEs that are going to be effected by the MODIFIER."
  `(progn (defclass ,name ,direct-superclasses
            ,direct-slots
            ,@options)
          (apply #'register-superclass-to-ctypes '(,name ,@ctypes))
          (ensure-ctypes '(,@ctypes))))

;; Tagging

(defclass tag ()
  ((name :initarg :name :reader tag-name)
   (slug :initarg :slug :reader tag-slug)
   (url  :initarg :url)))

(defmethod initialize-instance :after ((tag tag) &key)
  (with-slots (url slug) tag
    (setf url (compute-url nil slug 'tag-index))))

(defun make-tag (str)
  "Takes a string and returns a TAG instance with a name and slug."
  (let ((trimmed (string-trim " " str)))
    (make-instance 'tag :name trimmed :slug (slugify trimmed))))

(defun tag-slug= (a b)
  "Test if the slugs for tag A and B are equal."
  (string= (tag-slug a) (tag-slug b)))

;; Slugs

(defun slug-char-p (char &key (allowed-chars (list #\- #\~)))
  "Determine if CHAR is a valid slug (i.e. URL) character."
  ;; use the first char of the general unicode category as kind of
  ;; hyper general category
  (let ((cat (char (cl-unicode:general-category char) 0))
        (allowed-cats (list #\L #\N))) ; allowed Unicode categories in URLs
        (cond
          ((member cat allowed-cats)   t)
          ((member char allowed-chars) t)
          (t nil))))

(defun unicode-space-p (char)
  "Determine if CHAR is a kind of whitespace by unicode category means."
  (char= (char (cl-unicode:general-category char) 0) #\Z))

(defun slugify (string)
  "Return a version of STRING suitable for use as a URL."
  (let ((slugified (remove-if-not #'slug-char-p
                                  (substitute-if #\- #'unicode-space-p string))))
	  (if (zerop (length slugified))
        (error "Post title '~a' does not contain characters suitable for a slug!" string )
        slugified)))

;; Content Types

(defclass content ()
  ((url  :initarg :url  :reader page-url)
   (date :initarg :date :reader content-date)
   (file :initarg :file :reader content-file)
   (tags :initarg :tags :reader content-tags)
   (text :initarg :text :reader content-text))
  (:default-initargs :tags nil :date nil))

(defmethod initialize-instance :after ((object content) &key)
  (with-slots (tags) object
    (when (stringp tags)
      (setf tags (mapcar #'make-tag (cl-ppcre:split "," tags))))))

(defun parse-initarg (line)
  "Given a metadata header, LINE, parse an initarg name/value pair from it."
  (let ((name (string-upcase (subseq line 0 (position #\: line))))
        (match (nth-value 1 (scan-to-strings "[a-zA-Z]+:\\s+(.*)" line))))
    (when match
      (list (make-keyword name) (aref match 0)))))

(defun parse-metadata (stream)
  "Given a STREAM, parse metadata from it or signal an appropriate condition."
  (flet ((get-next-line (input)
           (string-trim '(#\Space #\Newline #\Tab) (read-line input nil))))
    (unless (string= (get-next-line stream) (separator *config*))
      (error "The file, ~a, lacks the expected header: ~a" (file-namestring stream) (separator *config*)))
    (loop for line = (get-next-line stream)
       until (string= line (separator *config*))
       appending (parse-initarg line))))

(defun read-content (file)
  "Returns a plist of metadata from FILE with :text holding the content."
  (flet ((slurp-remainder (stream)
           (let ((seq (make-string (- (file-length stream)
                                      (file-position stream)))))
             (read-sequence seq stream)
             (remove #\Nul seq))))
    (with-open-file (in file :external-format :utf-8)
      (let ((metadata (parse-metadata in))
            (content (slurp-remainder in))
            (filepath (enough-namestring file (repo-dir *config*))))
        (append metadata (list :text content :file filepath))))))

;; Helper Functions

(defun tag-p (tag obj)
  "Test if OBJ is tagged with TAG."
  (let ((tag (if (typep tag 'tag) tag (make-tag tag))))
    (member tag (content-tags obj) :test #'tag-slug=)))

(defun month-p (month obj)
  "Test if OBJ was written in MONTH."
  (search month (content-date obj)))

(defun by-date (content)
  "Sort CONTENT in reverse chronological order."
  (sort content #'string> :key #'content-date))

(defun find-content-by-path (path)
  "Find the CONTENT corresponding to the file at PATH."
  (find path (find-all 'content) :key #'content-file :test #'string=))

(defgeneric render-text (text format)
  (:documentation "Render TEXT of the given FORMAT to HTML for display.")
  (:method (text (format (eql :html)))
    text)
  (:method (text (format (eql :md)))
    (let ((3bmd-code-blocks:*code-blocks* t))
      (with-output-to-string (str)
        (3bmd:parse-string-and-print-to-stream text str)))))
