(in-package :coleslaw)

(defclass author ()
  ((name :initform nil
         :accessor author-name)
   (url :initform nil
        :accessor author-url)
   (ip :initform nil
       :accessor author-ip)))

(defclass comment ()
  ((id :initform nil
       :accessor comment-id)
   (post :initform nil
         :accessor comment-post)
   (author :initform nil
           :accessor comment-author)
   (timestamp :initform nil
              :accessor comment-timestamp)
   (content :initform nil
            :accessor comment-content)
   (parent :initform nil
           :accessor comment-parent)))

(defgeneric make-comment (post author timestamp content
                          parent &key &allow-other-key)
  (:documentation "Create a COMMENT with the given data."))

(defgeneric add-comment (comment post-id)
  (:documentation "Add COMMENT to POST-ID."))

(defgeneric delete-comment (comment post-id)
  (:documentation "Delete COMMENT from POST-ID."))

(defgeneric render-comments (post-id)
  (:documentation "Render the comments for POST-ID."))

(defgeneric find-comments (post-id)
  (:documentation "Find the comments for POST-ID."))
