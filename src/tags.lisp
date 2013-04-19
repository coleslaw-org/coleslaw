(in-package :coleslaw)

(defclass tag ()
  ((name :initform nil :initarg :name :accessor tag-name)
   (slug :initform nil :Initarg :slug :accessor tag-slug)))

(defun tag-name= (tag-1 tag-2)
  (string= (tag-name tag-1) (tag-name tag-2)))

(defun tag-slug= (tag-1 tag-2)
  (string= (tag-slug tag-1) (tag-slug tag-2)))

