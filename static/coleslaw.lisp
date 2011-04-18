(in-package :coleslaw)

(defvar *site-root* nil
  "A string representing the base URL of the site,
e.g. \"http://blog.redlinernotes.com/\".")

(defvar *site-title* nil
  "A string containing the title of the site,
e.g. \"Improved Means for Achieving Deterioriated Ends\".")

(defvar *site-credits* nil
  "A string containing the credits of the site,
e.g. \"Brit Butler (year)\".")

(defvar *site-license* nil
  "A string containing the (optional) license of the site,
e.g. \"CC-BY-SA\". Otherwise, standard copyright is assumed.")

(defvar *output-directory* nil
  "The path where the compiled coleslaw site will be output.")

(defvar *input-directory* nil
  "The directory which will be watched for new posts.")

(defun static-init ()
  (setf *storage* (make-hash-table))
  (loop for table in '(:authors :comments :posts :indices :credentials)
     do (unless (gethash table *storage*)
          (setf (gethash table *storage*) (make-hash-table)))))

(defmethod start-coleslaw (&rest options)
  )

(defmethod stop-coleslaw (&rest options)
  )

(defmethod get-credentials (name)
  (gethash name (gethash :credentials *storage*)))

(defmethod set-credentials (name credentials)
  (setf (gethash name (gethash :credentials *storage*)) credentials))
