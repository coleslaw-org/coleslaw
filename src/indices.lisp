(in-package :coleslaw)

(defgeneric add-index (index id)
  (:documentation "Insert INDEX into *storage* with the given ID."))

(defgeneric remove-index (id)
  (:documentation "Remove the index matching ID from *storage*."))

(defgeneric render-index (id page)
  (:documentation "Generate the final HTML for the index with given ID."))

(defgeneric find-index (id)
  (:documentation "Retrieve the index matching ID from *storage*."))

(defgeneric index-url (id page)
  (:documentation "Return the URL for the PAGE of index with given ID."))
