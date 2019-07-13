# coleslaw

[![Build Status](https://travis-ci.org/kingcons/coleslaw.svg?branch=master)](https://travis-ci.org/kingcons/coleslaw)
[![Quicklisp](http://quickdocs.org/badge/coleslaw.svg)](http://quickdocs.org/coleslaw/)

<img src="https://raw.github.com/redline6561/coleslaw/master/themes/hyde/css/logo_medium.jpg" alt="coleslaw logo" align="right"/>

> [Czeslaw Milosz](http://blog.redlinernotes.com/tag/milosz.html) was the writer-in-residence at UNC c. 1992.
> I used to see him all the time at the Hardback Cafe, always sitting at a two-top
> drinking coffee, reading, writing, eating chips and salsa. I remember a gentleness
> behind the enormous bushy eyebrows and that we called him Coleslaw. - anon

Coleslaw is Flexible Lisp Blogware similar to [Frog](https://github.com/greghendershott/frog), [Jekyll](http://jekyllrb.com/), or [Hakyll](http://jaspervdj.be/hakyll/).

Have questions? 
- IRC in **#coleslaw** on Freenode!
- Subscribe to the mailing list [**coleslaw@common-lisp.net**](https://mailman.common-lisp.net/listinfo/coleslaw).

## Features

* Git for storage
* RSS and Atom feeds
* Markdown Support with Code Highlighting provided by [colorize](http://www.cliki.net/colorize)
  * Currently supports: Common Lisp, Emacs Lisp, Scheme, C, C++, Java, Python, Erlang, Haskell, Obj-C, Diff.

* A [Plugin API](http://github.com/redline6561/coleslaw/blob/master/docs/plugin-api.md) and [**plugins**](http://github.com/redline6561/coleslaw/blob/master/docs/plugin-use.md) for...
  * Static Pages
  * Sitemap generation
  * Incremental builds
  * Analytics via Google or [Piwik](http://www.piwik.org)
  * Comments via [Disqus](http://disqus.com/) or [isso](http://posativ.org/isso)
  * Hosting via [Github Pages](https://pages.github.com/) or [Amazon S3](http://aws.amazon.com/s3/)
  * Embedding [gfycats](http://gfycat.com/)
  * [Tweeting](http://twitter.com/) about new posts
  * Using LaTeX via [Mathjax](http://mathjax.org/)
  * Writing posts in ReStructured Text
  * Importing posts from [Wordpress](http://wordpress.org/)
  * Code Highlighting via [Pygments](http://pygments.org/) instead of [colorize](http://www.cliki.net/colorize)

## Example Sites

See the [wiki](https://github.com/redline6561/coleslaw/wiki/Blogroll) for a list of coleslaw-powered blogs.

## Hacking

A core goal of *coleslaw* is to be both pleasant to read and easy to
hack on and extend. If you want to understand the internals and bend
*coleslaw* to do new and interesting things, I strongly encourage you
to read the [Hacker's Guide to Coleslaw][hackers]. You'll find some
current **TODO** items towards the bottom.

[hackers]: https://github.com/redline6561/coleslaw/blob/master/docs/hacking.md

## Installation

Coleslaw should run on any conforming Common Lisp implementation but
testing is primarily done on [SBCL](http://www.sbcl.org/) and
[CCL](http://ccl.clozure.com/).

Coleslaw can either be run **manually** on a local machine or
triggered **automatically** on git push to a server.  If you want a
server install, run these commands on your server _after_ setting up a
[git bare repo](http://git-scm.com/book/en/Git-on-the-Server-Setting-Up-the-Server).
Otherwise, run the commands on your local machine.

1. Install a Common Lisp implementation (we recommend SBCL) and
   [Quicklisp](http://quicklisp.org/).
2. Place a config file for coleslaw in your `$HOME` directory. If you
   want to run multiple blogs with coleslaw, you can keep each blog's
   config file in that blog's repo.  Feel free to copy and edit the
   [example config][ex_config] or consult the [config docs][conf_docs]
   to create one from scratch.
3. This step depends on whether you're setting up a local or server install.
   * Server Install: Copy and `chmod +x` the
     [example post-receive hook][post_hook] to your blog's bare repo.
   * Local Install:  Just run the following commands in the
     REPL whenever you're ready to regenerate your blog:
     ```
     (ql:quickload :coleslaw)
     (coleslaw:main "/path/to/my/blog/")
     ```
4. Optionally, point the web server of your liking at your config-specified
   `:deploy-dir`. Or "deploy-dir/.curr" if the `versioned` plugin is enabled.
5. If you use Emacs, consider installing
   [coleslaw-mode](https://github.com/equwal/coleslaw-mode) to author your
   posts.

Now just write posts, git commit and build by hand or by push.

[ex_config]: https://github.com/redline6561/coleslaw/blob/master/examples/example.coleslawrc
[conf_docs]: https://github.com/redline6561/coleslaw/blob/master/docs/config.md
[post_hook]: https://github.com/redline6561/coleslaw/blob/master/examples/example.post-receive

## The Content Format

Coleslaw expects content to have a file extension matching the class
of the content. (I.e. `.post` for blog posts, `.page` for static
pages, etc.)

There should also be a metadata header on all files
starting and ending with the config-specified `:separator`, ";;;;;" by
default. Example:

```
;;;;;
title: foo
tags: bar, baz
date: yyyy-mm-dd hh:mm:ss
format: html (for raw html) or md (for markdown)
excerpt: Can also be extracted from content (see :excerpt-sep config param)
;;;;;
your post
```

Posts require the `title:` and `format:` fields.
Pages require the `title:` and `url:` fields.

To omit a field, simply do not have the line present, empty lines and
fields (e.g. "tags:" followed by whitespace) will be ignored.

## Theming

Two themes are provided: hyde, the default, and readable (based on
[bootswatch readable](http://bootswatch.com/readable/)).

A guide to creating themes for coleslaw lives
[here](https://github.com/redline6561/coleslaw/blob/master/docs/themes.md).
