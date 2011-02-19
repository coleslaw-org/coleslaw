(in-package :coleslaw)

(defgeneric create-index (name ids dates urls)
  (:documentation "Create an index in *storage* with the given NAME, post IDs,
post DATEs and post URLs."))

(defgeneric get-index (name)
  (:documentation "Retrieve the index matching NAME from *storage*."))

(defgeneric add-to-index (index post)
  (:documentation "Add the given POST to INDEX."))

(defgeneric remove-from-index (index post)
  (:documentation "Remove the given POST from INDEX."))

(defgeneric render-index (index)
  (:documentation "Generate the final HTML for INDEX."))
