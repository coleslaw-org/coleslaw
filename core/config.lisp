(in-package :coleslaw)

(defparameter *credentials* nil
  "A map of names to credentials, potentially an alist. Names should be
keywords or symbols, credentials should be dotted pairs.")

(defgeneric get-credentials (name)
  (:documentation "Retrieve the credentials keyed by NAME from *credentials*."))

(defgeneric set-credentials (name credentials)
  (:documentation "Store the given CREDENTIALS in *credentials* under NAME."))
