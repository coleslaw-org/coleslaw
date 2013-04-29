(in-package :coleslaw)

(defparameter *content* (make-hash-table :test #'equal)
  "A hash table to store all the site content and metadata.")

(defclass tag ()
  ((name :initform nil :initarg :name :accessor tag-name)
   (slug :initform nil :Initarg :slug :accessor tag-slug)))

(defun make-tag (str)
  "Takes a string and returns a TAG instance with a name and slug."
  (let ((trimmed (string-trim " " str)))
    (make-instance 'tag :name trimmed :slug (slugify trimmed))))

(defun tag-slug= (a b)
  "Test if the slugs for tag A and B are equal."
  (string= (tag-slug a) (tag-slug b)))

(defclass content ()
  ((tags :initform nil :initarg :tags :accessor content-tags)
   (slug :initform nil :initarg :slug :accessor content-slug)
   (date :initform nil :initarg :date :accessor content-date)
   (text :initform nil :initarg :text :accessor content-text)))

(defun construct (content-type args)
  "Create an instance of CONTENT-TYPE with the given ARGS."
  (apply 'make-instance content-type args))

(defun tag-p (tag obj)
  "Test if OBJ is tagged with TAG."
  (member tag (content-tags obj) :test #'tag-slug=))

(defun month-p (month obj)
  "Test if OBJ was written in MONTH."
  (search month (content-date obj)))

(defgeneric publish (content-type)
  (:documentation "Write pages to disk for all content of the given CONTENT-TYPE."))

(defun read-content (file)
  "Returns a plist of metadata from FILE with :text holding the content as a string."
  (flet ((slurp-remainder (stream)
           (let ((seq (make-string (- (file-length stream)
                                      (file-position stream)))))
             (read-sequence seq stream)
             (remove #\Nul seq)))
         (parse-field (str)
           (nth-value 1 (cl-ppcre:scan-to-strings "[a-zA-Z]+: (.*)" str)))
         (field-name (line)
           (make-keyword (string-upcase (subseq line 0 (position #\: line)))))
         (read-tags (str)
           (mapcar #'make-tag (cl-ppcre:split "," str))))
    (with-open-file (in file)
      (unless (string= (read-line in) ";;;;;")
        (error "The provided file lacks the expected header."))
      (let ((meta (loop for line = (read-line in nil)
                     until (string= line ";;;;;")
                     appending (list (field-name line)
                                     (aref (parse-field line) 0))))
            (content (slurp-remainder in)))
        (setf (getf meta :tags) (read-tags (getf meta :tags)))
        (append meta (list :text content))))))

(defun find-all (content-type)
  "Return a list of all instances of a given CONTENT-TYPE."
  (loop for val being the hash-values in *content*
     when (typep val content-type) collect val))

(defun purge-all (content-type)
  "Remove all instances of CONTENT-TYPE from *content*."
  (dolist (obj (find-all content-type))
    (remhash (content-slug obj) *content*)))

(defun discover (content-type)
  "Load all content of the given CONTENT-TYPE from disk."
  (purge-all content-type)
  (let ((file-type (string-downcase (princ-to-string content-type))))
    (do-files (file (repo *config*) file-type)
      (let ((obj (construct content-type (read-content file))))
        (if (gethash (content-slug obj) *content*)
            (error "There is already existing content with the slug ~a."
                   (content-slug obj))
            (setf (gethash (content-slug obj) *content*) obj))))))

(defmacro do-ctypes (&body body)
  "Iterate over the subclasses of CONTENT performing BODY with ctype lexically
bound to the current subclass."
  (alexandria:with-gensyms (ctypes)
    `(let ((,ctypes (closer-mop:class-direct-subclasses (find-class 'content))))
       (loop for ctype in (mapcar #'class-name ,ctypes) do ,@body))))

(defun load-content ()
  "Load all content stored in the blog's repo."
  (do-ctypes (discover ctype)))

(defun by-date (content)
  "Sort CONTENT in reverse chronological order."
  (sort content #'string> :key #'content-date))

(defun slug-char-p (char)
  "Determine if CHAR is a valid slug (i.e. URL) character."
  (or (char<= #\0 char #\9)
      (char<= #\a char #\z)
      (char<= #\A char #\Z)
      (member char '(#\_ #\-))))

(defun slugify (string)
  "Return a version of STRING suitable for use as a URL."
  (remove-if-not #'slug-char-p (substitute #\- #\Space string)))
