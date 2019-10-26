(defpackage :coleslaw-cli
  (:use :cl :trivia))
  
(in-package :coleslaw-cli)

(defun setup-coleslawrc (user &aux (path (merge-pathnames ".coleslawrc")))
  "Set up the default .coleslawrc file in the current directory."
  (with-open-file (s path :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format t "~&Generating ~a ...~%" path)
    ;; odd formatting in this source code because emacs has problem detecting the parenthesis inside a string
    (format s ";;; -*- mode : lisp -*-~%(~
 ;; Required information
 :author \"~a\"                         ;; to be placed on post pages and in the copyright/CC-BY-SA notice
 :deploy-dir \"deploy/\"                ;; for Coleslaw's generated HTML to go in
 :domain \"https://~a.github.com\"      ;; to generate absolute links to the site content
 :routing ((:post           \"posts/~~a\") ;; to determine the URL scheme of content on the site
           (:tag-index      \"tag/~~a\")
           (:month-index    \"date/~~a\")
           (:numeric-index  \"~~d\")
           (:feed           \"~~a.xml\")
           (:tag-feed       \"tag/~~a.xml\"))
 :title \"Improved Means for Achieving Deteriorated Ends\" ;; a site title
 :theme \"hyde\"                        ;; to select one of the themes in \"coleslaw/themes/\"
 
 ;; Optional information
 :excerpt-sep \"<!--more-->\"           ;; to set the separator for excerpt in content 
 :feeds (\"lisp\")
 :plugins ((analytics :tracking-code \"foo\")
           (disqus :shortname \"my-site-name\")
           ; (incremental)  ;; *Remove comment to enable incremental builds.
           (mathjax)
           (sitemap)
           (static-pages)
           ; (versioned)    ;; *Remove comment to enable symlinked, timestamped deploys.
          )
 :sitenav ((:url \"http://~a.github.com/\" :name \"Home\")
           (:url \"http://twitter.com/~a\" :name \"Twitter\")
           (:url \"http://github.com/~a\" :name \"Code\")
           (:url \"http://soundcloud.com/~a\" :name \"Music\")
           (:url \"http://redlinernotes.com/docs/talks/\" :name \"Talks\"))
 :staging-dir \"/tmp/coleslaw/\"  ;; for Coleslaw to do intermediate work, default: \"/tmp/coleslaw\"
)

;; * Prerequisites described in plugin docs."
            user
            user
            user
            user
            user
            user)))

(defun copy-theme (which &optional (target which))
  "Copy the theme named WHICH into the blog directory and rename it into TARGET"
  (format t "~&Copying themes/~a ...~%" which)
  (if (probe-file (format nil "themes/~a" which))
      (format t "~&  themes/~a already exists.~%" which)
      (progn
        (ensure-directories-exist "themes/" :verbose t)
        (uiop:run-program `("cp" "-v" "-r"
                                 ,(namestring (coleslaw::app-path "themes/~a/" which))
                                 ,(namestring (merge-pathnames (format nil "themes/~a" target))))))))

(defun setup (&optional (user (uiop:getenv "USER")))
  (setup-coleslawrc user)
  (copy-theme "hyde" "default"))

(defun read-rc (&aux (path (merge-pathnames ".coleslawrc")))
  (with-open-file (s (if (probe-file path)
                         path
                         (merge-pathnames #p".coleslawrc" (user-homedir-pathname))))
    (read s)))

(defun new (&optional (type "post") name)
  (let ((sep (getf (read-rc) :separator ";;;;;")))
    (multiple-value-match (get-decoded-time)
      ((second minute hour date month year _ _ _)
       (let* ((name (or name
                        (format nil "~a-~2,,,'0@a-~2,,,'0@a" year month date)))
              (path (merge-pathnames (make-pathname :name name :type type))))
         (with-open-file (s path
                            :direction :output :if-exists :error :if-does-not-exist :create)
           (format s "~
~a
title: ~a
tags: bar, baz
date: ~a-~2,,,'0@a-~2,,,'0@a ~2,,,'0@a:~2,,,'0@a:~2,,,'0@a
format: md
~:[~*~;URL: pages/~a.html~%~]~
~a

<!-- **** your post here (remove this line) **** -->
<!-- format: could be 'html' (for raw html) or 'md' (for markdown).  -->

Here is my content.

<!--more-->

Excerpt separator can also be extracted from content.
Add `excerpt: <string>` to the above metadata.
Excerpt separator is `<!--more-->` by default.
"
                   sep
                   name
                   year month date hour minute second
                   (string= type "page") name
                   sep)
           (format *error-output* "~&Created a ~a \"~a\".~%" type name)
           (format t "~&~a~%" path)))))))

(defun generate ()
  (coleslaw:main *default-pathname-defaults*))

(defun preview (&optional (path (getf (read-rc) :deploy-dir)))
  ;; clack depends on the global binding of *default-pathname-defaults*.
  (let ((oldpath *default-pathname-defaults*))
    (unwind-protect
         (progn
           (when path
             (setf *default-pathname-defaults* (truename path)))
           (format t "~%Starting a Clack server at ~a~%" path)
           (clack:clackup
            (lack:builder
             :accesslog
             (:static :path (lambda (p)
                              (if (char= #\/ (alexandria:last-elt p))
                                  (concatenate 'string p "index.html")
                                  p)))
             #'identity)
            :use-thread nil))
      (setf *default-pathname-defaults* oldpath))))

;; code from fs-watcher

(defun mtime (pathname)
  "Returns the mtime of a pathname"
  (when (ignore-errors (probe-file pathname))
    (file-write-date pathname)))

(defun dir-contents (pathnames test)
  (remove-if-not test
                 ;; uiop:slurp-input-stream
                 (uiop:run-program `("find" ,@(mapcar #'namestring pathnames))
                                   :output :lines)))

(defun run-loop (pathnames mtimes callback delay)
  "The main loop constantly polling the filesystem"
  (loop
    (sleep delay)
    (map nil
         #'(lambda (pathname)
             (let ((mtime (mtime pathname)))
               (unless (eql mtime (gethash pathname mtimes))
                 (funcall callback pathname)
                 (if mtime
                   (setf (gethash pathname mtimes) mtime)
                   (remhash pathname mtimes)))))
         pathnames)))

(defun watch (&optional (source-path *default-pathname-defaults*))
  (format t "~&Start watching! : ~a~%" source-path)
  (let ((pathnames 
         (dir-contents (list source-path)
                       (lambda (p) (not (equal "fasl" (pathname-type p))))))
        (mtimes (make-hash-table)))
    (dolist (pathname pathnames)
      (setf (gethash pathname mtimes) (mtime pathname)))
    (ignore-errors
      (run-loop pathnames
                mtimes
                (lambda (pathname)
                  (format t "~&Changes detected! : ~a~%" pathname)
                  (finish-output)
                  (handler-case
                      (coleslaw:main source-path)
                    (error (c)
                      (format *error-output* "something happened... ~a" c))))
                1))))

(defun watch-preview (&optional (source-path *default-pathname-defaults*))
  (when (member :swank *features*)
    (warn "FIXME: This command does not do what you intend from a SLIME session."))
  (ignore-errors
    (uiop:run-program
     ;; The hackiness here is because clack fails? to handle? SIGINT correctly when run in a threaded mode
     `("sh" "-c" ,(format nil "coleslaw watch ~a &~
                               coleslaw preview &~
                               jobs -p;~
                               trap \"kill $(jobs -p)\" EXIT;~
                               wait" source-path))
     :output :interactive
     :error-output :interactive)))

(defun help ()
  (format *error-output* "


Coleslaw, a Flexible Lisp Blogware.
Written by: Brit Butler <redline6561@gmail.com>.
Distributed by BSD license.

Command Line Syntax:

coleslaw setup [NAME]                --- Sets up a new .coleslawrc file in the current directory.
coleslaw copy-theme THEME [TARGET]   --- Copies the installed THEME in coleslaw to the current directory with a different name TARGET.
coleslaw new [TYPE] [NAME]           --- Creates a new content file with the correct format. TYPE defaults to 'post', NAME defaults to the current date.
coleslaw generate                    --- Generates the static html according to .coleslawrc .
coleslaw preview [DIRECTORY]         --- Runs a preview server at port 5000. DIRECTORY defaults to the deploy directory (described in .coleslawrc).
coleslaw watch [DIRECTORY]           --- Watches the given directory and generates the site when changes are detected. Defaults to the current directory.
coleslaw                             --- Shorthand of 'coleslaw generate'.
coleslaw -h                          --- Show this help

Corresponding REPL commands are available in coleslaw-cli package.

```lisp
  (ql:quickload :coleslaw-cli)
  (coleslaw-cli:setup            &optional name)
  (coleslaw-cli:copy-theme theme &optional target)
  (coleslaw-cli:new              &optional type name)
  (coleslaw-cli:generate)
  (coleslaw-cli:preview &optional directory)
  (coleslaw-cli:watch   &optional directory)
```

Examples:

* set up a blog

    mkdir yourblog ; cd yourblog
    git init
    coleslaw setup
    git commit -a -m 'initial repo'

* Copy the base theme to the current directory for modification

    coleslaw copy-theme hyde mytheme

* Create a post

    coleslaw new

* Create a page (static page)

    coleslaw new page

* Generate a site

    coleslaw generate
    # or just:
    coleslaw

* Preview a site

    coleslaw preview
    # or
    coleslaw preview .

"
          ))

(defun main (&rest argv)
  (declare (ignorable argv))
  (match argv
    ((list* "setup" rest)
     (apply #'setup rest))
    ((list* "preview" rest)
     (apply #'preview rest))
    ((list* "watch" rest)
     (apply #'watch rest))
    ((list* "watch-preview" rest)
     (apply #'watch-preview rest))
    ((list* "new" rest)
     (apply #'new rest))
    ((or nil (list "generate"))
     (generate))
    ((list* "copy-theme" rest)
     (apply #'copy-theme rest))
    ((list* (or "-v" "--version") _)
     )
    ((list* (or "-h" "--help") _)
     (help))))

(when (member :swank *features*)
  (help))
