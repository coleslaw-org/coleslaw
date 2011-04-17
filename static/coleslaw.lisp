(in-package :coleslaw)

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
