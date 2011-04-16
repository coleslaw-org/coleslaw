(in-package :coleslaw)

(defclass author ()
  ((name :initform nil :initarg :name
         :accessor author-name)
   (url :initform nil :initarg :url
        :accessor author-url)
   (ip :initform nil :initarg :ip
       :accessor author-ip)))

(defclass comment ()
  ((id :initform nil :initarg :id
       :accessor comment-id)
   (post :initform nil :initarg :post
         :accessor comment-post)
   (author :initform nil :initarg :author
           :accessor comment-author)
   (timestamp :initform nil :initarg :timestamp
              :accessor comment-timestamp)
   (content :initform nil :initarg :content
            :accessor comment-content)
   (parent :initform nil :initarg :parent
           :accessor comment-parent)))

(defgeneric make-comment (post author timestamp content
                          parent &key id &allow-other-keys)
  (:documentation "Create a COMMENT with the given data."))

(defgeneric add-comment (comment post-id)
  (:documentation "Add COMMENT to POST-ID."))

(defgeneric delete-comment (comment post-id)
  (:documentation "Delete COMMENT from POST-ID."))

(defgeneric render-comments (post-id)
  (:documentation "Render the comments for POST-ID."))

(defgeneric find-comments (post-id)
  (:documentation "Find the comments for POST-ID."))
