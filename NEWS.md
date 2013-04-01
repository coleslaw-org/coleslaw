## Changes for 0.9.1 (2013-04-xx):

* Make ATOM and RSS templates a separate "generic" theme. (thanks @woudshoo!)

## Changes for 0.9 (2013-02-20):

* INCOMPATIBLE CHANGE: All :plugins in .coleslawrc must be lists. (i.e. (mathjax) not mathjax)
* Add support for analytics via Google.
* Add support for Restructured Text via cl-docutils.
* Add support for deploying to Amazon S3.
* Add a heroku plugin to ease hunchentoot deployments. (thanks @jsmpereira!)
* Ensure coleslaw exits after MAIN. Fixes issue #13.
* Greatly improved docs for the various plugins and plugin API.

## Changes for 0.8 (2013-01-06):

* Add support for new [content types](http://blog.redlinernotes.com/posts/Lessons-from-Coleslaw.html).
* Support for [Multi-site Publishing](http://blub.co.za/posts/Adding-multi-site-support-to-Coleslaw.html).
* CCL and Atom feed bugfixes.
* Major code refactor and docs update.

## Changes for 0.7 (2012-09-20):

* Add commenting support via Disqus plugin.
* Add formal plugin API with per-page predicate support. (aka "injections")
* Note jsmpereira's [coleslaw heroku package](https://github.com/jsmpereira/coleslaw-heroku) in README.
* Support for RSS feeds of arbitrary tags, e.g. "lisp" posts.

## Changes for 0.6.5 (2012-09-12):

* Add support for ATOM feeds.
* Add support for a sitenav in coleslawrc configs.
* Template and rendering cleanup.
* Miscellaneous deployment improvements.

## Changes for 0.6 (2012-08-29):

* Support Markdown in core rather than as a plugin.
* Improve documentation + README.
* Copious bugfixes and code cleanups.

## Changes for 0.5 (2012-08-22):

* Initial release.
