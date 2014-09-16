## Changes for 0.9.6 (2014-09-17):

* **SITE-BREAKING CHANGE**: Coleslaw now defaults to a "basic" deploy
  instead of the previous symlinked, timestamped deploy strategy.
  To retain the previous behavior, add `(versioned)` to your config's
  `:plugins` list.
* **SITE-BREAKING CHANGE**: Custom themes will be broken by a change
  to URL handling. Previously, we were hand-constructing URLs in the
  templates. All site objects now store their URL in an instance slot.
  In general, hrefs should be of the form `<a href="{$config.domain}/{$obj.url}"> ...</a>`.
* Changes to `:routing` would previously break links in the templates
  but now work seamlessly due to the updated URL handling.
* **Docs**: Improved README and Theming docs. New Config File docs.
* Loading content is more robust when empty lines or metadata are passed.
  Thanks to @PuercoPop for the bug report and preliminary fix.
* The config `:repo` option is now deprecated as its value has become
  a required argument to `coleslaw:main`. The value passed to `main`
  will override the config value going forward.
* Improved handling of directories and error-reporting when they
  don't exist is available thanks to @PuercoPop.
* The templates are now HTML5 valid thanks to @Ferada.
* Fixed a bug where RSS/Atom tag feeds were being published multiple times.

## Changes for 0.9.5 (2014-06-13):

* A plugin for Incremental builds, cutting runtime for generating
  medium to large sites roughly in half!
* A Twitter plugin to tweet about your new posts. Thanks to @PuercoPop!
* Config options for the HTML lang and charset attributes. Thanks to @ryumei!
* Coleslaw now exports a `get-updated-files` function which can be
  used to get a list of file-status/file-name pairs that were changed
  in the last git push. There is also an exported `find-content-by-path`
  function to retrieve content objects from the above file-name. These
  were used by both the Twitter and Incremental plugins.
* The usual bugfixes, performance improvements, and documentation tweaks.

## Changes for 0.9.4 (2014-05-05):

* **SITE-BREAKING CHANGE**: Coleslaw now supports user-defined routing.
  Instead of hard-coding the paths various content types are stored at,
  they **must** be specified in the configuration file (.coleslawrc).
  Just copy the `:routing` key from the [example][example.rc] to
  get the old behavior.
* **SITE-BREAKING CHANGE**: Coleslaw's multi-site support has changed.
  Instead of having a single .coleslawrc in the user's home directory
  that has sections for multiple repos, a .coleslawrc may be included
  *in* the blog repo itself. If no .coleslawrc is found in the repo,
  it is loaded from the user's home directory instead.
* Coleslaw no longer expects a particular repo layout. Use whatever
  directory hierarchy you like.
* New Content Type Plugin: Static Pages, accepting a title, url, and
  optionally tags and a date. All files with a `.page` extension are
  compiled as static pages and reuse the POST template.
  To enable Static Pages, add `(static-pages)` to the `:plugins`
  section of your config.
* Coleslaw now allows content without a date or tags. Note that POSTs
  without a date will still show up in the reverse chronological
  indexes at the very end.
* Fixed an embarrassing escaping bug in our last quicklisp release.

## Changes for 0.9.3 (2014-04-16):

* **INCOMPATIBLE CHANGE**: `page-path` and the `blog` config class are no longer exported.
* **INCOMPATIBLE CHANGE**: `render-content` has been renamed `render-text` for clarity.
* New Docs: [A Hacker's Guide to Coleslaw][hacking_guide] and [Themes][theming_guide]!
* A new theme *readable* based on bootswatch readable, courtesy of @rmoritz!
* Posts may have an author to support multi-user blogs courtesy of @tychoish.
* Fixes to the ReStructuredText plugin courtesy of @tychoish.
* UTF-8 fixes for config files and site content courtesy of @cl-ment.
* Fix timestamps in the sitemap plugin courtesy of @woudshoo.

## Changes for 0.9.2 (2013-05-11):

* **INCOMPATIBLE CHANGE**: Renamed staging, deploy config options staging-dir, deploy-dir.
* A plugin for Github Pages support. (thanks @mrordinaire!)
* A new and improved implementation of tags. (thanks @woudshoo!)
* A THEME-DOES-NOT-EXIST error is raised when the theme can't be found.

## Changes for 0.9.1 (2013-04-10):

* Added a PREVIEW function for REPL use.
* Make ATOM and RSS templates a separate "generic" theme. (thanks @woudshoo!)
* Fixed bug where repeatedly loading plugins caused them to appear in the page more than once. (thanks @woudshoo!)
* Fixes to spacing in navigation and tagsoup. (thanks @woudshoo!)

## Changes for 0.9 (2013-02-20):

* **INCOMPATIBLE CHANGE**: All :plugins in .coleslawrc must be lists. (i.e. (mathjax) not mathjax)
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

[hacking_guide]:  https://github.com/redline6561/coleslaw/blob/master/docs/hacking.md
[theming_guide]:  https://github.com/redline6561/coleslaw/blob/master/docs/themes.md
[example.rc]: https://github.com/redline6561/coleslaw/blob/master/examples/example.coleslawrc
