(in-package :coleslaw)

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
      (error "The file lacks the expected header: ~a" (separator *config*)))
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
            (filepath (enough-namestring file (repo *config*))))
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
