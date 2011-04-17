(in-package :coleslaw)

(defmethod find-index (id)
  (gethash id (gethash :indices *storage*)))

(defmethod add-index (index id)
  (setf (find-index id) index))

(defmethod remove-index (id)
  (setf (find-index id) nil))

(defmethod render-index (index)
  )
