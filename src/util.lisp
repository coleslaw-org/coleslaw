(in-package :coleslaw)


(define-condition coleslaw-condition ()
  ())

(define-condition field-missing (error coleslaw-condition)
  ((field-name :initarg :field-name :reader missing-field-field-name)
   (file :initarg :file :reader missing-field-file
         :documentation "The path of the file where the field is missing."))
  (:report
   (lambda (c s)
     (format s "~A: The required field ~A is missing."
             (missing-field-file c)
             (missing-field-field-name c)))))

(defmacro assert-field (field-name content)
  `(when (not (slot-boundp ,content ,field-name))
     (error 'field-missing
            :field-name ,field-name
            :file (content-file ,content))))

(defun construct (class-name args)
  "Create an instance of CLASS-NAME with the given ARGS."
  (apply 'make-instance class-name args))

;; Thanks to bknr-web for this bit of code.
(defun all-subclasses (class)
  "Return a list of all the subclasses of CLASS."
  (let ((subclasses (closer-mop:class-direct-subclasses class)))
    (append subclasses (loop for subclass in subclasses
                          nconc (all-subclasses subclass)))))

(defmacro do-subclasses ((var class) &body body)
  "Iterate over the subclasses of CLASS performing BODY with VAR
lexically bound to the current subclass."
  (alexandria:with-gensyms (klasses)
    `(let ((,klasses (all-subclasses (find-class ',class))))
       (loop for ,var in ,klasses do ,@body))))

(defmacro do-files ((var path &optional extension) &body body)
  "For each file under PATH, run BODY. If EXTENSION is provided, only run
BODY on files that match the given extension."
  (alexandria:with-gensyms (extension-p)
    `(flet ((,extension-p (file)
              (string= (pathname-type file) ,extension)))
       (cl-fad:walk-directory ,path (lambda (,var) ,@body)
                              :follow-symlinks nil
                              :test (if ,extension
                                        #',extension-p
                                        (constantly t))))))

(define-condition directory-does-not-exist (error)
  ((directory :initarg :dir :reader dir))
  (:report (lambda (c stream)
             (format stream "The directory '~A' does not exist" (dir c)))))

(defun (setf getcwd) (path)
  "Change the operating system's current directory to PATH."
  (setf path (ensure-directory-pathname path))
  (unless (and (uiop:directory-exists-p path)
               (uiop:chdir path))
    (error 'directory-does-not-exist :dir path))
  path)

(defmacro with-current-directory (path &body body)
  "Change the current directory to PATH and execute BODY in
an UNWIND-PROTECT, then change back to the current directory."
  (alexandria:with-gensyms (old)
    `(let ((,old (getcwd)))
       (unwind-protect (progn
                         (setf (getcwd) ,path)
                         ,@body)
         (setf (getcwd) ,old)))))

(defun exit ()
  ;; KLUDGE: Just call UIOP for now. Don't want users updating scripts.
  "Exit the lisp system returning a 0 status code."
  (uiop:quit))

(defun fmt (fmt-str args)
  "A convenient FORMAT interface for string building."
  (apply 'format nil fmt-str args))

(defun rel-path (base path &rest args)
  "Take a relative PATH and return the corresponding pathname beneath BASE.
If ARGS is provided, use (fmt path args) as the value of PATH."
  (merge-pathnames (fmt path args) base))

(defun app-path (path &rest args)
  "Return a relative path beneath coleslaw."
  (apply 'rel-path coleslaw-conf:*basedir* path args))

(defun repo-path (path &rest args)
  "Return a relative path beneath the repo being processed."
  (apply 'rel-path (repo-dir *config*) path args))

(defun run-program (program &rest args)
  "Take a PROGRAM and execute the corresponding shell command. If ARGS is provided,
use (fmt program args) as the value of PROGRAM."
  (inferior-shell:run (fmt program args) :show t))

(defun take-up-to (n seq)
  "Take elements from SEQ until all elements or N have been taken."
  (subseq seq 0 (min (length seq) n)))

(defun write-file (path text)
  "Write the given TEXT to PATH. PATH is overwritten if it exists and created
along with any missing parent directories otherwise."
  (ensure-directories-exist path)
  (with-open-file (out path
                   :direction :output
                   :if-exists :supersede
                   :if-does-not-exist :create
                   :external-format :utf-8)
    (write text :stream out :escape nil)))

(defun get-updated-files (&optional (revision *last-revision*))
  "Return a plist of (file-status file-name) for files that were changed
in the git repo since REVISION."
  (flet ((split-on-whitespace (str)
           (cl-ppcre:split "\\s+" str)))
    (let ((cmd (format nil "git diff --name-status ~A HEAD" revision)))
      (mapcar #'split-on-whitespace (inferior-shell:run/lines cmd)))))

(defun rsync-installed-p (&optional (test-args "--version"))
  (block escape
    (handler-case (run-program "rsync ~A" test-args)
      (t () (return-from escape nil)))
    t))

(defun run-lines (dir &rest programs)
  "Runs some programs, in a directory."
  (mapc (lambda (line)
          (run-program "cd ~A && ~A" dir line))
        programs))

(defun run-rsync (fmt-args &rest args)
  (run-program "rsync ~a ~a"
               (if (and (rsync-passfile *config*)
                        (which-sshpass *config*))
                   "--rsh=\"/usr/bin/sshpass -f /home/jose/.backup-pass ssh -o StrictHostKeyChecking=no\""
                   "")
               (apply #'format nil fmt-args args)))

(defun path-move (from to)
  "Move things, or copy with rsync."
  (if (rsync-installed-p)
      (run-rsync "--delete -lavz ~a ~a"
                 from
                 to)
      (run-program "mv ~a ~a" from to)))

(defun update-symlink (path target)
  (run-program "ln -sfn ~a ~a" target path))

(defun rsync-ensure-directories-exist (path)
  "Ensure directories exist, but not if using rsync."
  (when (not (rsync-installed-p))
    (ensure-directories-exist path)))

