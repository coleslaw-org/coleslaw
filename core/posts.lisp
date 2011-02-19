(in-package :coleslaw)

(defgeneric add-post (id title timestamp permalink tags
                      content &key &allow-other-keys)
  (:documentation "Create a post with the given ID, TITLE, TIMESTAMP,
PERMALINK, TAGS and CONTENT and add it to *storage*."))

(defgeneric prettify-code (post)
  (:documentation "Ensure that any escaped code in POST is prettified."))

(defgeneric prettify-math-p (post)
  (:documentation "Returns T if post needs MathJAX loaded, NIL otherwise."))

(defgeneric render-post (post)
  (:documentation "Generate the final HTML for POST."))

(defgeneric remove-post (post)
  (:documentation "Remove POST from *storage* and, if necessary,
update the running site."))
