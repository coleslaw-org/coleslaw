(in-package :coleslaw)

(defclass comment ()
  ((id :initarg :id
       :accessor id)
   (post-id :initarg :post-id
            :accessor post-id)
   (author-ip :initarg :author-ip
              :accessor author-ip)
   (author-name :initarg :author-name
                :accessor author-name)
   (author-url :initarg :author-url
               :accessor author-url)
   (timestamp :initarg :timestamp
              :accessor timestamp)
   (content :initarg :content
            :accessor content)
   (parent :initarg :parent
           :accessor parent))
  (:metaclass dao-class))
