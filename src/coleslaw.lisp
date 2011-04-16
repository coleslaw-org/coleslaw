(in-package :coleslaw)

(defgeneric start-coleslaw (&rest options)
  (:documentation "Start the coleslaw server with any specified OPTIONS."))

(defgeneric stop-coleslaw (&rest options)
  (:documentation "Stop the coleslaw server with any specified OPTIONS."))

(defparameter *storage* nil
  "A db-spec for postmodern or a hash-table cache. It is expected that
*storage* has methods for each Generic Function in coleslaw implemented.")

(defgeneric get-credentials (name)
  (:documentation "Retrieve the credentials keyed by NAME from *storage*."))

(defgeneric set-credentials (name credentials)
  (:documentation "Store the given CREDENTIALS in *storage* under NAME."))
