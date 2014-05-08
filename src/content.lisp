(in-package :coleslaw)

;; Tagging

(defclass tag ()
  ((name :initarg :name :reader tag-name)
   (slug :initarg :slug :reader tag-slug)))

(defun make-tag (str)
  "Takes a string and returns a TAG instance with a name and slug."
  (let ((trimmed (string-trim " " str)))
    (make-instance 'tag :name trimmed :slug (slugify trimmed))))

(defun tag-slug= (a b)
  "Test if the slugs for tag A and B are equal."
  (string= (tag-slug a) (tag-slug b)))

;; Slugs

(defun slug-char-p (char)
  "Determine if CHAR is a valid slug (i.e. URL) character."
  (or (char<= #\0 char #\9)
      (char<= #\a char #\z)
      (char<= #\A char #\Z)
      (member char '(#\_ #\-))))

(defun slugify (string)
  "Return a version of STRING suitable for use as a URL."
  (remove-if-not #'slug-char-p (substitute #\- #\Space string)))

;; Content Types

(defclass content ()
  ((file :initform nil :initarg :file :accessor content-file)
   (tags :initform nil :initarg :tags :accessor content-tags)
   (slug :initform nil :initarg :slug :accessor content-slug)
   (date :initform nil :initarg :date :accessor content-date)
   (text :initform nil :initarg :text :accessor content-text)))

(defmethod initialize-instance :after ((object content) &key)
  (with-accessors ((tags content-tags)) object
    (when (stringp tags)
      (setf tags (mapcar #'make-tag (cl-ppcre:split "," tags))))))

(defun read-content (file)
  "Returns a plist of metadata from FILE with :text holding the content as a string."
  (flet ((slurp-remainder (stream)
           (let ((seq (make-string (- (file-length stream)
                                      (file-position stream)))))
             (read-sequence seq stream)
             (remove #\Nul seq)))
         (parse-field (str)
           (nth-value 1 (cl-ppcre:scan-to-strings "[a-zA-Z]+:\\s+(.*)" str)))
         (field-name (line)
           (make-keyword (string-upcase (subseq line 0 (position #\: line))))))
    (with-open-file (in file :external-format '(:utf-8))
      (unless (string= (read-line in) (separator *config*))
        (error "The provided file lacks the expected header."))
      (let ((meta (loop for line = (read-line in nil)
                     until (string= line (separator *config*))
                     appending (list (field-name line)
                                     (aref (parse-field line) 0))))
            (filepath (enough-namestring file (repo *config*)))
            (content (slurp-remainder in)))
        (append meta (list :text content :file filepath))))))

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
