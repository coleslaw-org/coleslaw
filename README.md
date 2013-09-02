# coleslaw

<img src="https://raw.github.com/redline6561/coleslaw/master/themes/hyde/css/logo_medium.jpg" alt="coleslaw logo" align="right"/>

> [Czeslaw Milosz](http://blog.redlinernotes.com/tag/milosz.html) was the writer-in-residence at UNC c. 1992.
> I used to see him all the time at the Hardback Cafe, always sitting at a two-top
> drinking coffee, reading, writing, eating chips and salsa. I remember a gentleness
> behind the enormous bushy eyebrows and that we called him Coleslaw. - anon

Coleslaw aims to be flexible blog software suitable for replacing a single-user static site compiler such as Jekyll.

## Features
* Git for storage
* RSS and Atom feeds!
* Markdown Support with Code Highlighting provided by [colorize](http://www.cliki.net/colorize).
  * Currently supports: Common Lisp, Emacs Lisp, Scheme, C, C++, Java, Python, Erlang, Haskell, Obj-C, Diff.
* [Multi-site publishing](http://rmoritz.github.io/articles/coleslaw-multi-site/) support.

* A [Plugin API](http://github.com/redline6561/coleslaw/blob/master/docs/plugin-api.md) and [**plugins**](http://github.com/redline6561/coleslaw/blob/master/docs/plugin-use.md) for...
  * Comments via Disqus
  * Analytics via Google
  * Hosting via Github Pages
  * Deploying to Amazon S3
  * Using LaTeX (inside pairs of $$) via Mathjax
  * Using ReStructured Text
  * Sitemap generation
  * Importing posts from wordpress

* There is also a [Heroku buildpack](https://github.com/jsmpereira/coleslaw-heroku) maintained by Jose Pereira.
* Example sites: [redlinernotes](http://redlinernotes.com/blog/) and [Nothing Really Matters](http://ironhead.xs4all.nl/).

## Installation
This software should be portable to any conforming Common Lisp implementation but this guide will assume SBCL is installed. Testing has also been done on CCL.
Server side setup:

1. Setup git and create a bare repo as shown [here](http://git-scm.com/book/en/Git-on-the-Server-Setting-Up-the-Server).
2. Install Lisp and [Quicklisp](http://quicklisp.org/).
3. ```wget -c https://raw.github.com/redline6561/coleslaw/master/examples/single-site.coleslawrc -O ~/.coleslawrc``` # and edit as necessary
4. ```wget -c https://raw.github.com/redline6561/coleslaw/master/examples/example.post-receive -O your-blog.git/hooks/post-receive``` # and edit as necessary
5. ```chmod +x your-blog/.git/hooks/post-receive```
6. Create or clone your blog repo locally. Add your server as a remote with ```git remote add prod git@my-host.com:path/to/repo.git```
7. Point the web server of your choice at the symlink /path/to/deploy-dir/.curr/

Now whenever you push a new commit to the server, coleslaw will update your blog automatically! You may need to git push -u prod master the first time.

## The Post Format
Coleslaw expects post files to be formatted as follows:
```
;;;;;
title: foo
tags: bar, baz
date: yyyy-mm-dd hh:mm:ss
format: html (for raw html) or md (for markdown)
;;;;;
your post
```

## Theming
A default theme, hyde, is provided. Themes are made using Google's closure-template and the source for [hyde](https://github.com/redline6561/coleslaw/tree/master/themes/hyde) should be simple and instructive until I can provide better docs.
