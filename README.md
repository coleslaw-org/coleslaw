# coleslaw

[![Build Status](https://travis-ci.org/coleslaw-org/coleslaw.svg?branch=master)](https://travis-ci.org/kingcons/coleslaw)
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

See the [wiki](https://github.com/redline6561/coleslaw/wiki/Example-sites) for a list of coleslaw-powered blogs.

Coleslaw should run on any conforming Common Lisp implementations but
testing is primarily done on [SBCL](http://www.sbcl.org/) and [CCL](http://ccl.clozure.com/).

## Features

* Git for storage
* RSS/Atom feeds
* Themes
* A [Plugin API](docs/plugin-api.md) and [**plugins**](docs/plugin-use.md) for...

| plugins                                                | plugins                                      | plugins                                               |
|--------------------------------------------------------|----------------------------------------------|-------------------------------------------------------|
| Sitemap generation                                     | Incremental builds                           | Analytics via Google or [Piwik](http://www.piwik.org) |
| Comments via [Disqus](http://disqus.com/)              | Comments via [isso](http://posativ.org/isso) | Hosting via  [Amazon S3](http://aws.amazon.com/s3/)   |
| Hosting via [Github Pages](https://pages.github.com/)  | Embedding [gfycats](http://gfycat.com/)      | [Tweeting](http://twitter.com/) about new posts       |
| [Mathjax](http://mathjax.org/)                         | Posts in ReStructured Text                   | [Wordpress](http://wordpress.org/) import             |
| [Pygments](http://pygments.org/)                       | [colorize](http://www.cliki.net/colorize)    |                                                       |


## Installation/Tutorial

<!-- Don't let the first user select from multiple choises -->

Step 1: Install this library.

With [Roswell](https://roswell.github.io/),
``` sh
$ ros install coleslaw
$ export PATH="$HOME/.roswell/bin:$PATH" # If you haven't done this before
```

or

``` lisp
(ql:quickload :coleslaw-cli)
```


Step 2: Initialize your blog repository.

``` sh
$ mkdir yourblog ; cd yourblog
$ git init
$ coleslaw setup
```
``` lisp
(coleslaw-cli:setup)
```

Step 3: Write a post file in the current directory.
The file should contain a certain metadata, so use the `coleslaw new` command,
which instantiates a correct file for you.

``` sh
$ coleslaw new
Created a post 2017-11-06.post .
```
``` lisp
(coleslaw-cli:new "post")
```

Step 4: Generate the site from those post files.
The result goes to the `deploy/` subdirectory.

``` sh
$ coleslaw
```
``` lisp
(coleslaw-cli:generate)
```

Step 5: You can also launch a server...

``` sh
$ coleslaw preview
```
``` lisp
(coleslaw-cli:preview)
```

Step 6: and watch the file system to automatically regenerate the site!

``` sh
$ coleslaw watch          # or even better,
$ coleslaw watch-preview 
```
``` lisp
(coleslaw-cli:watch)      ;; watch-preview does not work on REPL right now
```

For further customization, e.g. adding a new plugin, developing a new plugin, changing the deploy option, or creating a new theme,
see the [config docs](docs).

We provide three default themes: hyde, the default, and readable (based on
[bootswatch readable](http://bootswatch.com/readable/)).

A core goal of *coleslaw* is to be both pleasant to read and easy to
hack on and extend. If you want to understand the internals and bend
*coleslaw* to do new and interesting things, I strongly encourage you
to read the [Hacker's Guide to Coleslaw][hackers]. You'll find some
current **TODO** items towards the bottom.

[hackers]: docs/hacking.md
