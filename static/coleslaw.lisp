(in-package :coleslaw)

(defvar *site-root* nil
  "A string representing the base URL of the site,
e.g. \"http://blog.redlinernotes.com\".")

(defvar *site-title* nil
  "A string containing the title of the site,
e.g. \"Improved Means for Achieving Deterioriated Ends\".")

(defvar *site-credits* nil
  "A string containing the credits of the site,
e.g. \"Brit Butler (year)\".")

(defvar *site-license* nil
  "A string containing the (optional) license of the site,
e.g. \"CC-BY-SA\". Otherwise, standard copyright is assumed.")

(defvar *site-navigation* nil
  "A string of HTML describing a navbar or similar structure.")

(defvar *output-dir* nil
  "The path where the compiled coleslaw site will be output.")

(defvar *input-dir* nil
  "The directory which will be watched for new posts.")

(defun static-init ()
  (setf *storage* (make-hash-table))
  (loop for table in '(:authors :comments :posts :credentials)
     do (setf (gethash table *storage*) (make-hash-table)))
  (setf (gethash :indices *storage*) (make-hash-table :test #'equal)))

(defmethod start-coleslaw (&rest options)
  )

(defmethod stop-coleslaw (&rest options)
  )

(defmethod get-credentials (name)
  (gethash name (gethash :credentials *storage*)))

(defmethod set-credentials (name credentials)
  (setf (gethash name (gethash :credentials *storage*)) credentials))

(defmethod add-injection ((str string) location)
  (pushnew str (gethash location *storage*) :test #'string))

(defmethod remove-injection ((str string) location)
  (setf (gethash location *storage*)
        (remove str (gethash location *storage*) :test #'string=)))

(defmethod render-page (content)
  (let ((result (funcall (find-symbol "BASE" (theme-package))
                         (list :title *site-title*
                               :siteroot *site-root*
                               :head-inject (apply #'concatenate 'string
                                                   (gethash :head *storage*))
                               :navigation *site-navigation*
                               :content content
                               :body-inject (apply #'concatenate 'string
                                                   (gethash :body *storage*))
                               :license *site-license*
                               :credits *site-credits*))))
    result))

(defun write-post (post)
  (let ((filepath (merge-pathnames
                   (concatenate 'string (year-month (post-date post))
                                "/" (escape (post-title post)) ".html")
                   *output-dir*)))
    (ensure-directories-exist filepath)
    (with-open-file (out filepath :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
      (write-string (render-page (render-post (post-id post))) out))))

(defun write-index (index)
  (ensure-directories-exist
   (cl-fad:pathname-as-directory (merge-pathnames (index-id index)
                                                  *output-dir*)))
  (let* ((count (length (index-posts index)))
         (pages (ceiling (/ count 10))))
    (loop for page from 1 to pages do
         (let ((filepath (merge-pathnames
                          (concatenate 'string (index-id index)
                                       "/" (write-to-string page) ".html")
                          *output-dir*)))
           (with-open-file (out filepath :direction :output
                            :if-exists :supersede :if-does-not-exist :create)
             (write-string (render-page (render-index (index-id index) page)) out))))))

(defun render-site ()
  (flet ((copy-dir (from to)
           (cl-fad:walk-directory from
             (lambda (file)
               (let ((name (concatenate 'string
                                        (pathname-name file) "."
                                        (pathname-type file))))
                 (cl-fad:copy-file file (merge-pathnames name to)))))))
    (when (cl-fad:directory-exists-p *output-dir*)
      (cl-fad:delete-directory-and-files *output-dir*))
    (ensure-directories-exist *output-dir*)
    (let ((css-dir (merge-pathnames "css/" *output-dir*))
          (static-dir (merge-pathnames "static/" *output-dir*)))
      (ensure-directories-exist css-dir)
      (ensure-directories-exist static-dir)
      ;; TODO: Copy-dir dies if the directories aren't there...
      (copy-dir (merge-pathnames "css/" *theme-dir*) css-dir)
      (copy-dir (merge-pathnames "static/" *input-dir*) static-dir))
    (loop for post being the hash-values in (gethash :posts *storage*)
       do (write-post post))
    (loop for index being the hash-values in (gethash :indices *storage*)
       do (write-index index))))
