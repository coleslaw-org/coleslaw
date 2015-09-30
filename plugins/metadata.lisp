(eval-when (:compile-toplevel :load-toplevel)
  (ql:quickload 's-xml))

(defpackage :coleslaw-metadata
  (:use :cl :coleslaw)
  (:import-from :coleslaw
                #:title
                #:domain
                #:tag-name
                #:page-url
                #:make-keyword
                #:content-tags
                #:content-text)
    (:import-from :s-xml
                #:xml-parser-state
                #:start-parse-xml)
    (:export #:enable))

(in-package :coleslaw-metadata)

(defmodifier metadata (post) ()
  ((keywords :initarg :keywords :reader keywords-of)
   (description :initarg :description :reader description-of)
   (image :initarg :image :reader image-of)
   (card :initarg :card :reader card-format)
   (creator :initarg :creator :reader creator-of))
  (:default-initargs
   :keywords nil
   :description nil
   :image nil
   :card nil
   :creator nil))

(defmethod initialize-instance :after ((object metadata) &key)
  (with-slots (card) object
    (setf card (if card (make-keyword (string-upcase card))))))

(defparameter *description-length* 200)

(defvar *metadata-header*
  "
<meta name=\"keywords\" content=\"~A\" />~@[
<meta name=\"description\" content=\"~A\" />~]~:[~*~*~;~:*
<meta name=\"twitter:site\" content=\"@~A\" />
<meta name=\"twitter:card\" content=\"~:[summary~;summary_large_image~]\" />~@[
<meta name=\"twitter:creator\" content=\"@~A\" />~]~]
<meta property=\"og:title\" content=\"~A\" />
<meta property=\"og:site_name\" content=\"~A\" />
<meta property=\"og:url\" content=\"~A\" />
<meta property=\"og:description\" content=\"~A\" />~@[
<meta property=\"og:image\" content=\"~A\" />~]
")

(defun remove-markup (text)
  (with-input-from-string (in text)
    (let* ((state (make-instance 'xml-parser-state
                                 :text-hook #'(lambda (string seed) (cons string seed))))
           (result (start-parse-xml in state)))
      (apply #'concatenate 'string (nreverse result)))))

(defun shorten-text (text)
  (if (< *description-length* (length text))
      (subseq text 0 (- *description-length* 1)) text))

(defun compile-description (text)
  (shorten-text (remove #\" (remove-markup text))))

(defun root-relative-url-p (url)
  (eq (elt url 0) #\/))

(defun compile-url (url)
  (if (root-relative-url-p url)
      (concatenate 'string (domain *config*) url)
      url))

(defun compile-metadata (post twitter card)
  (format nil *metadata-header*
          (or (keywords-of post)
              (format nil "~{~A~^, ~}" (mapcar #'tag-name (content-tags post))))
          (description-of post)
          twitter
          (eq (or (card-format post) card) :image)
          (creator-of post)
          (title-of post)
          (title *config*)
          (concatenate 'string (domain *config*) "/" (namestring (page-url post)))
          (or (description-of post)
              (compile-description (content-text post)))
          (when (image-of post) (compile-url (image-of post)))))

(defun enable (&key twitter card)
  (flet ((inject-p (x)
           (when (typep x 'metadata) (compile-metadata x twitter card))))
    (add-injection #'inject-p :head)))
