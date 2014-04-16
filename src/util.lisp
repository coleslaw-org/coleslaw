(in-package :coleslaw)

(defun construct (class-name args)
  "Create an instance of CLASS-NAME with the given ARGS."
  (apply 'make-instance class-name args))

(defmacro do-subclasses ((var class) &body body)
  "Iterate over the subclasses of CLASS performing BODY with VAR
lexically bound to the current subclass' class-name."
  (alexandria:with-gensyms (klasses all-subclasses)
    `(labels ((,all-subclasses (class)
                (let ((subclasses (closer-mop:class-direct-subclasses class)))
                  (append subclasses (loop for subclass in subclasses
                                        nconc (,all-subclasses subclass))))))
       (let ((,klasses (,all-subclasses (find-class ',class))))
         (loop for ,var in ,klasses do ,@body)))))

(defmacro do-files ((var path &optional extension) &body body)
  "For each file on PATH, run BODY. If EXTENSION is provided, only run BODY
on files that match the given extension."
  (alexandria:with-gensyms (ext)
    `(dolist (,var (cl-fad:list-directory ,path))
       ,@(if extension
             `((let ((,ext (pathname-type ,var)))
                 (when (and ,ext (string= ,ext ,extension))
                   ,@body)))
             `,body))))

(defmacro with-current-directory (path &body body)
  "Change the current OS directory to PATH and execute BODY in
an UNWIND-PROTECT, then change back to the current directory."
  (alexandria:with-gensyms (old)
    `(let ((,old (current-directory)))
       (unwind-protect (progn
                         (setf (current-directory) ,path)
                         ,@body)
         (setf (current-directory) ,old)))))

(defun current-directory ()
  "Return the operating system's current directory."
  #+sbcl (sb-posix:getcwd)
  #+ccl (ccl:current-directory)
  #+ecl (si:getcwd)
  #+cmucl (unix:unix-current-directory)
  #+clisp (ext:cd)
  #-(or sbcl ccl ecl cmucl clisp) (error "Not implemented yet."))

(defun (setf current-directory) (path)
  "Change the operating system's current directory to PATH."
  #+sbcl (sb-posix:chdir path)
  #+ccl (setf (ccl:current-directory) path)
  #+ecl (si:chdir path)
  #+cmucl (unix:unix-chdir (namestring path))
  #+clisp (ext:cd path)
  #-(or sbcl ccl ecl cmucl clisp) (error "Not implemented yet."))

(defun exit ()
  "Exit the lisp system returning a 0 status code."
  #+sbcl (sb-ext:quit)
  #+ccl (ccl:quit)
  #+ecl (si:quit)
  #+cmucl (ext:quit)
  #+clisp (ext:quit)
  #-(or sbcl ccl ecl cmucl clisp) (error "Not implemented yet."))

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

(defun run-program (program &rest args)
  "Take a PROGRAM and execute the corresponding shell command. If ARGS is provided,
use (fmt program args) as the value of PROGRAM."
  (inferior-shell:run (fmt program args) :show t))

(defun update-symlink (path target)
  "Update the symlink at PATH to point to TARGET."
  (run-program "ln -sfn ~a ~a" target path))

(defun take-up-to (n seq)
  "Take elements from SEQ until all elements or N have been taken."
  (subseq seq 0 (min (length seq) n)))
