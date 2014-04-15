(in-package :coleslaw)

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

(defun tag-p (tag obj)
  "Test if OBJ is tagged with TAG."
  (member tag (content-tags obj) :test #'tag-slug=))

(defun month-p (month obj)
  "Test if OBJ was written in MONTH."
  (search month (content-date obj)))

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
    (with-open-file (in file :external-format '(:utf-8))
      (unless (string= (read-line in) (separator *config*))
        (error "The provided file lacks the expected header."))
      (let ((meta (loop for line = (read-line in nil)
                     until (string= line (separator *config*))
                     appending (list (field-name line)
                                     (aref (parse-field line) 0))))
            (content (slurp-remainder in)))
        (setf (getf meta :tags) (read-tags (getf meta :tags)))
        (append meta (list :text content))))))

(defun load-content ()
  "Load all content stored in the blog's repo."
  (do-subclasses (ctype content)
    (discover ctype)))

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
