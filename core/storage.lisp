(in-package :coleslaw)

(defparameter *storage* nil
  "A db-spec for postmodern or a hash-table cache of metadata.
It is expected that *storage* has methods for each Generic Function
in coleslaw-core implemented.")

(defgeneric find-by-id (id)
  (:documentation "Retrieve a POST object from *storage* matching ID."))

(defgeneric find-by-tag (tag)
  (:documentation "Retrieve all POST objects from *storage* tagged with TAG."))

(defgeneric find-by-date (date)
  (:documentation "Retrieve all POST objects from *storage* matching DATE."))

(defgeneric find-by-range (start end)
  (:documentation "Retrieve all POST objects from *storage* with ids between
START and END."))
