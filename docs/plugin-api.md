# General Use

1. A lisp file should be created in coleslaw's ```plugins``` directory.
2. Any necessary lisp libraries not loaded by coleslaw should be included like so:

    ```(eval-when (:compile-toplevel :load-toplevel) (ql:quickload '(foo bar)))```

3. A package should be created for the plugin code like so:

    ```(defpackage :coleslaw-$NAME (:use :cl) (:export #:enable) ...)```

    where $NAME is the pathname-name of the lisp file. (eg. `:coleslaw-disqus` for `disqus.lisp`)
4. An enable function should be present even if it's a no-op. Any work to enable the plugin is done there.


# Extension Points

* **New functionality via JS**, for example the Disqus and Mathjax
  plugins. In this case, the plugin's `enable` function should call
  [`add-injection`](http://redlinernotes.com/docs/coleslaw.html#add-injection_func)
  with an injection and a keyword. The injection itself is a list of
  the string to insert and a lambda or function that can be called on
  a content instance to determine whether the injection should be
  included on the page. The keyword specifies whether the injected
  text goes in the HEAD or BODY element. The
  [Disqus plugin](http://github.com/redline6561/coleslaw/blob/master/plugins/disqus.lisp)
  is a good example of this.

* **New markup formats**, for example the
  [ReStructuredText plugin](http://github.com/redline6561/coleslaw/blob/master/plugins/rst.lisp),
  can be created by definining an appropriate `render-text`
  method. The method takes `text` and `format` arguments and is
  [EQL-specialized](http://www.gigamonkeys.com/book/object-reorientation-generic-functions.html#defmethod)
  on the format. Format should be a keyword matching the file
  extension (or `pathname-type`) of the markup format.
  (eg. `:rst` for ReStructuredText)

* **New hosting options**, for example the
  [Amazon S3 plugin](http://github.com/redline6561/coleslaw/blob/master/plugins/s3.lisp),
  can be created by definining a `deploy :after` method. The method
  takes a staging directory, likely uninteresting in the `:after`
  stage. But by importing `*config*` from the coleslaw package and
  getting its deploy location with `(deploy-dir *config*)` a number of
  interesting options become possible.

* **New content types**, for example the
  [static page content type](http://github.com/redline6561/coleslaw/blob/master/plugins/static-pages.lisp),
  can be created by definining a subclass of CONTENT along with a
  template, and `render`, `page-url`, and `publish` methods.
  The PAGE content type cheats a bit by reusing the existing POST template.

* **New service integrations**, for example crossposting to
  twitter/facebook/tumblr/livejournal/etc, is also possible by
  adding an :after hook to the deploy method. The hook can iterate
  over the results of the `get-updated-files` to crosspost any new content.
  The [Twitter plugin](https://github.com/redline6561/coleslaw/blob/master/plugins/twitter.lisp)
  is a good example of this.
