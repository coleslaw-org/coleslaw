(in-package :coleslaw)

(defclass post ()
  ((id :initform nil :initarg :id
       :accessor post-id)
   (title :initform nil :initarg :title
          :accessor post-title)
   (tags :initform nil :initarg :tags
         :accessor post-tags)
   (date :initform nil :initarg :date
         :accessor post-date)
   (content :initform nil :initarg :content
            :accessor post-content)
   (aliases :initform nil :initarg :aliases
            :accessor post-aliases)))

(defgeneric make-post (title tags date content
                       &key id aliases &allow-other-keys)
  (:documentation "Create a POST with the given data."))

(defgeneric add-post (post id)
  (:documentation "Insert a post into *storage* with the given ID."))

(defgeneric remove-post (id)
  (:documentation "Remove a post from *storage* matching ID."))

(defgeneric render-post (id)
  (:documentation "Generate the final HTML for the post with given ID."))

(defgeneric find-post (id)
  (:documentation "Retrieve a post from *storage* matching ID."))

(defgeneric find-by-tag (tag)
  (:documentation "Retrieve all posts from *storage* tagged with TAG."))

(defgeneric find-by-date (date)
  (:documentation "Retrieve all posts from *storage* matching DATE."))

(defgeneric find-by-range (start end)
  (:documentation "Retrieve all posts from *storage* with ids between
START and END."))

(defgeneric post-url (id)
  (:documentation "Return the URL for the post with the given ID."))
