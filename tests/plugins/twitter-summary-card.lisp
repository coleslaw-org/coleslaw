(defpackage :summary-card-tests
  (:use :cl :coleslaw :stefil))

(in-package :summary-card-tests)

(defsuite summary-cards)
(in-suite summary-cards)

;; TODO: Create a fixture to either load a mocked config or load set of plugins.
;; Then wrap these tests to use that fixture. Then add these to defsystem, setup
;; general test run with other packages.

(coleslaw::enable-plugin :twitter-summary-card)

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
    (is (null (cl-ppcre:scan "twitter:author" summary-card)))))

(deftest summary-card-with-twitter-handle ()
  (let ((summary-card (summary-card *short-post* "@PuercoPop")))
    (is (cl-ppcre:scan "twitter:author" summary-card))))

(deftest summary-card-trims-long-post ()
  (let ((summary-card (summary-card *long-post* nil)))
    (multiple-value-bind ())
    ;; (scan "twitter:description\" content=\"(.*)\"" summary-card)
    summary-card))
