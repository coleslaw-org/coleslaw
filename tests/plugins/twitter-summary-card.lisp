(stefil:define-test-package :twitter-summary-card-tests
  (:use :cl :coleslaw
             :cl-ppcre))

(in-package :twitter-summary-card-tests)
;;;; 
;; To run tests first eval the plugin code inside this package and then execute
;; (twitter-summary-card-test:run-package-tests)
;;;


(defvar *short-post*
    (make-instance 'post :title "hai" :text "very greetings" :format "html"))

(defvar *long-post* 
    (make-instance 'post :title "What a Wonderful World"
                   :text "I see trees of green, red roses too.  I see them
bloom, for me and you.  And I think to myself, what a wonderful world.

I see skies of blue, 
And clouds of white. 
The bright blessed day, 
The dark sacred night. 
And I think to myself, 
What a wonderful world. 

The colors of the rainbow, 
So pretty in the sky. 
Are also on the faces, 
Of people going by, 
I see friends shaking hands. 
Saying, \"How do you do?\" 
They're really saying, 
\"I love you\". 

I hear babies cry, 
I watch them grow, 
They'll learn much more, 
Than I'll ever know. 
And I think to myself, 
What a wonderful world. 

Yes, I think to myself, 
What a wonderful world. " :format "html"))

(deftest summary-card-sans-twitter-handle ()
  (let ((summary-card (summary-card *short-post* nil)))
    (is (null (scan "twitter:author" summary-card)))))

(deftest summary-card-with-twitter-handle ()
  (let ((summary-card (summary-card *short-post* "@PuercoPop")))
    (is (scan "twitter:author" summary-card))))

(deftest summary-card-trims-long-post ()
  (let ((summary-card (summary-card *long-post* nil)))
    ;; (scan "twitter:description\" content=\"(.*)\"" summary-card)
    summary-card))


