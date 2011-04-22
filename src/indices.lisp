(in-package :coleslaw)

(defclass index ()
  ((id :initform nil :initarg :id
       :accessor index-id)
   (posts :initform nil :initarg :posts
          :accessor index-posts)))

(defgeneric make-index (id posts)
  (:documentation "Create an INDEX with the given data."))

(defgeneric add-to-index (id post)
  (:documentation "Add POST to the index matching ID, creating INDEX if
it does not exist."))

(defgeneric remove-from-index (id post)
  (:documentation "Remove POST from the index matching ID, removing INDEX if
it holds no more posts."))

(defgeneric render-index (id page)
  (:documentation "Generate the final HTML for a given PAGE of the index ID."))

(defgeneric find-index (id)
  (:documentation "Retrieve the index matching ID from *storage*."))

(defgeneric index-url (id page)
  (:documentation "Return the URL for the PAGE of index with given ID."))
