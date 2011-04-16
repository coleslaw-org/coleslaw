(in-package :coleslaw)

(defgeneric add-index (index id)
  (:documentation "Insert INDEX into *storage* with the given ID."))

(defgeneric remove-index (id)
  (:documentation "Remove the index matching ID from *storage*."))

(defgeneric render-index (index)
  (:documentation "Generate the final HTML for INDEX."))

(defgeneric find-index (id)
  (:documentation "Retrieve the index matching ID from *storage*."))
