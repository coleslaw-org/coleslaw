## Coleslaw: A Hacker's Guide

Here we'll provide an overview of key concepts and technical decisions
in *coleslaw* and a few suggestions about future directions. Please
keep in mind that *coleslaw* was written on a lark when 3 friends had
the idea to each complete their half-dreamed wordpress replacement in
a week. Though it has evolved considerably since it's inception, like
any software some mess remains.

## Overall Structure

Conceptually, coleslaw processes a blog as follows:

1.  Coleslaw loads the user's config, then reads the blog repo loading
    any `.post` files or other content. CONTENT and INDEX objects are
    created from those files.

2.  The CONTENT and INDEX objects are then fed to the templating engine
    to produce HTML files in a config-specified staging directory,
    usually under `/tmp`.

3.  A deploy method (possibly customized via plugins) is called with the
    staging directory. It does whatever work is needed to make the
    generated HTML files (and any static content) visible to the web.

## A Note on Performance

A recent test on my Core i5 touting Thinkpad generated my 430-post blog
in 2.7 seconds. This averages out to about 6-7 milliseconds per piece of
content. However, about 400 of those 430 items were HTML posts from a
wordpress export, not markdown posts that require parsing with 3bmd.
I expect that 3bmd would be the main bottleneck on a larger site. It
would be worthwhile to see how well [cl-markdown][clmd] performs as
a replacement if this becomes an issue for users though we would lose
source highlighting from [colorize][clrz] and should also investigate
[pygments][pyg] as a replacement. Using the new [incremental][incf] plugin
reduced runtime to 1.36 seconds, almost cutting it in half.

## Core Concepts

### Plugins

**Coleslaw** strongly encourages extending functionality via plugins.
The Plugin API is well-documented and flexible enough for many use
cases. Do check the [API docs][api_docs] when contemplating a new
feature and see if a plugin would be appropriate.

### Templates and Theming

User configs are allowed to specify a theme. A theme consists of a
directory under "themes/" containing css, images, and at least
3 templates: Base, Index, and Post.

**Coleslaw** uses [cl-closure-template][closure_template]
exclusively for templating. **cl-closure-template** is a well
documented CL implementation of Google's Closure Templates. Each
template file should contain a namespace like
`coleslaw.theme.theme-name`.

Each template creates a lisp function in the theme's package when
loaded. These functions take a property list (or plist) as an argument
and return rendered HTML.  **Coleslaw** defines a helper called
`theme-fn` for easy access to the template functions. Additionally,
there are RSS, ATOM, and sitemap templates *coleslaw* uses automatically.
No need for individual themes to reimplement a standard, after all!

Unfortunately, it is not very pleasant to debug broken templates.
Efforts to remedy this are being pursued for the next release.
Two particular issues to note are transposed Closure commands,
e.g. "${foo}" instead of "{$foo}", and trying to use nonexistent
keys or slots which fails silently instead of producing an error.

### The Lifecycle of a Page

- `(progn
     (load-config "/my/blog/repo/path")
     (compile-theme (theme *config*)))`

Coleslaw first needs the config loaded and theme compiled,
as neither the blog location, the theme to use, and other
crucial information are not yet known.

- `(load-content)`

A page starts, obviously, with a file. When *coleslaw* loads your
content, it iterates over a list of content types (i.e. subclasses of
CONTENT).  For each content type, it iterates over all files in the
repo with a matching extension, e.g. ".post" for POSTs. Objects of the
appropriate class are created from each matching file and inserted
into the an in-memory data store. Then the INDEXes are created from
the loaded content and added to the data store.

- `(compile-blog dir)`

Compilation starts by ensuring the staging directory (`/tmp/coleslaw/`
by default) exists, cd'ing there, and copying over any necessary theme
assets. Then *coleslaw* iterates over all the content types and index
classes, rendering all of their instances and writing the HTML to disk.
After this, an 'index.html' symlink is created pointing to the first
reverse-chronological index.

- `(deploy dir)`

Finally, we move the staging directory to a path under the config's
`:deploy-dir`. If the versioned plugin is enabled, it is a timestamped
path and we delete the directory pointed to by the old '.prev' symlink,
point '.curr' at '.prev', and point '.curr' at our freshly built site.

### Blogs vs Sites

**Coleslaw** is blogware. When I designed it, I only cared that it
could replace my server's wordpress install. As a result, the code
until very recently was structured in terms of POSTs and
INDEXes. Roughly speaking, a POST is a blog entry and an INDEX is a
collection of POSTs or other content. An INDEX really only serves to
group a set of content objects on a page, it isn't content itself.

Content Types were added in 0.8 as a step towards making *coleslaw*
suitable for more use cases. Any subclass of CONTENT that implements
the *document protocol* counts as a content type. However, only POSTs
are currently included in the bundled INDEXes since there isn't yet a
formal relationship to determine which content types should be
included on which indexes. It is straightforward for users to implement
their own dedicated INDEX for new Content Types.

### The Document Protocol

The *document protocol* was born during a giant refactoring in 0.9.3.
Any object that will be rendered to HTML should adhere to the protocol.
Subclasses of CONTENT (content types) that implement the protocol will
be seamlessly picked up by *coleslaw* and included on the rendered site.

All current Content Types and Indexes implement the protocol faithfully.
It consists of 2 "class" methods, 2 instance methods, and an invariant.

There are also 5 helper functions provided that should prove useful in
implementing new content types.


**Class Methods**:

Class Methods don't *really* exist in Common Lisp, as methods are
defined on generic functions and not on the class itself. But since
it's useful to think about a Class as being responsible for its
instances in the case of a blog, we implement class methods by
eql-specializing on the class, e.g.

```lisp
(defmethod foo ((doc-type (eql (find-class 'bar))))
  ... )
```

- `discover`: Create instances for documents of the class and put them
  in the in-memory database with `add-document`.

  For CONTENT, this means checking the blog repo for any files with a
  matching extension and loading them from disk. If your class is a
  subclass of CONTENT, it inherits a pleasant default method for this.

  For INDEXes, this means iterating over any relevant CONTENT in the
  database, and creating INDEXes in the database that include that
  content.

- `publish`: Iterate over all instances of the class, rendering each
  one to HTML and writing it out to the staging directory on disk.


**Instance Methods**:

- `page-url`: Retrieve the relative path for the object on the site.
  The implementation of `page-url` is not fully specified. For most
  content types, we compute and store the path on the instance at
  initialization time making `page-url` just a reader method.

- `render`: A method that calls the appropriate template with `theme-fn`,
  passing it any needed arguments and returning rendered HTML.

**Invariants**:

- Any Content Types (subclasses of CONTENT) are expected to be stored in
  the site's git repo with the lowercased class-name as a file extension,
  i.e. (".post" for POST files).

**Protocol Helpers**:

- `add-document`: Add the document to *coleslaw*'s in-memory
  database. It will error if the `page-url` of the document is not
  unique. Such a hash collision represents content on the site being
  shadowed/overwritten. This should be used in your `discover` method.

- `delete-document`: Remove a document from *coleslaw*'s in-memory
  database. This is currently only used by the incremental compilation
  plugin.

- `write-document`: Write the document out to disk as HTML. It takes
  an optional template name and render-args to pass to the template.
  This should be used in your `publish` method.

- `find-all`: Return a list of all documents of the requested class.
  This is often used in the `publish` method to iterate over documents
  of a given type.

- `purge-all`: Remove all instances of the requested class from the DB.
  This is primarily used at the REPL or for debugging but it is also
  used in a `:before` method on `discover` to keep it idempotent.

### Current Content Types & Indexes

There are 5 INDEX subclasses at present: TAG-INDEX, MONTH-INDEX,
NUMERIC-INDEX, FEED, and TAG-FEED. Respectively, they support
grouping content by tags, publishing date, and reverse chronological
order. Feeds exist to special case RSS and ATOM generation.
Currently, there is only 1 content type: POST, for blog entries.
PAGE, a content type for static page support, is available as a plugin.

## Areas for Improvement (i.e. The Roadmap)

### TODO for 0.9.7

* Test suite improvements:
  * `load-content`/`read-content`/parsing
  * Content Discovery
  * Theme Compilation
  * Content Publishing
  * Common Plugins including Injections
* Add proper errors to read-content/load-content? Not just ignoring bad data. Line info, etc.
* Improved template debugging? "${" instead of "{$", static checks for valid slots, etc.
  At least a serious investigation into how such things might be provided.
* Some minor scripting conveniences with cl-launch? (Scaffold a post/page, Enable incremental, Build, etc).

### Assorted Cleanups

* Try to get tag-index urls out of the tags. Post templates use them.
* Profile/memoize find-all calls in **INDEX** `render` method.

### Real Error Handling

One reason Coleslaw's code base is so small is probably the
omission of any serious error handling. Trying to debug
coleslaw if there's a problem during a build is unpleasant
at best, especially for anyone not coming from the lisp world.

We need to start handling errors and reporting errors in ways
that are useful to the user. Example errors users have encountered:

1. Loading of Content. If `read-content` fails to parse a file, we
   should tell the user what file failed and why. We also should
   probably enforce more constraints about metadata. E.g. Empty
   metadata is not allowed/meaningful. Trailing space after separator, etc.
2. Trying to load content from the bare repo instead of the clone.
   i.e. Specifying the `:repo` in .coleslawrc as the bare repo.
   The README should clarify this point and the need for posts to be
   ".post" files.
3. Custom themes that try to access non-existent properties of content
   do not currently error. They just wind up returning whitespace.
   When the theme compiles, we should alert the user to any obvious
   issues with it.
4. Dear Lord it was miserable even debugging a transposed character error
   in one of the templates. "${foo}" instead of "{$foo}". But fuck supporting
   multiple templating backends I have enough problems. What can we do?

### Scripting Conveniences

It would be convenient to add command-line tools/scripts to run coleslaw,
set up the db for incremental builds, scaffold a new post, etc. for new users.
Fukamachi's Shelly, Xach's buildapp or Fare's cl-launch would be useful here. frog and hakyll are
reasonable points of inspiration for commands to offer.

#### Commands

This is a initial set of commands which will be used to implement the first coleslaw cli, feel free to contribute!
Imagine a executable `coleslaw`, the commands would be invoked like this: `coleslaw <commandname> <args>`

* `build` generates the site. Takes:
	* `--repo-dir`: first defaults to `~/.coleslawrc`'s `repo-dir` then to `.` and otherwise fails
* `clean` removes the files from `output-dir` and `staging-dir`. Takes:
	* `--repo-dir`: See above
* `rebuild` is a shortcut for `clean` and then `build`. Takes:
	* `--repo-dir`: See above
* `post` creates a new empty `.post` file. Takes:
	* `--repo-dir`: See above
	* `--title`: title (also used for generating file name)
	* `--date`: same as the header-key. If not given, current time is used.
	* `--format`: same as the header-key (optional, defaults to `md`)
	* …
* `serve` starts a hunchentoot serving the blog locally
* maybe `page` which is `post` for static sites.

Ideas for later:

* Deployment. It would be nice to have but user needs vary greatly
  and there are multiple deployment methods to support (heroku, s3,
  gh-pages, git, etc). This will require some careful thought.

### Plugin Constraints

There is no system for determining what plugins work together or
enforcing the requirements or constraints of any particular
plugin. That is to say, the plugins are not actually modular. They are
closer to controlled monkey-patching.

While adding a [real module system to common lisp][asdf3] is probably
out of scope, we might be able to add some kind of [contract library][qpq]
for implementing this functionality. At the very least, a way to check
some assertions and error out at plugin load time if they fail should be
doable. I might not be able to [make illegal states unrepresentable][misu],
but I can sure as hell make them harder to construct than they are now.

@PuercoPop has suggested looking into how [wookie does plugins][wookie].
It's much more heavyweight but might be worth looking into. If we go that
route, the plugin support code will be almost half the coleslaw core.
Weigh the tradeoffs carefully.

### New Content Type: Shouts!

I've also toyed with the idea of a content type called a SHOUT, which
would be used primarily to reference or embed other content, sort of a
mix between a retweet and a del.icio.us bookmark. We encounter plenty
of great things on the web. Most of mine winds up forgotten in browser
tabs or stored on twitter's servers. It would be cool to see SHOUTs as
a plugin, probably with a dedicated SHOUT-INDEX, and some sort of
oEmbed/embed.ly/noembed support.

### Better Content Types

Creating a new content type is both straightforward and doable as a
plugin. All that is really required is a subclass of CONTENT with
any needed slots, a template, a `render` method to call the template
with any needed options, a `page-url` method for layout, and a
`publish` method.

Unfortunately, this does not solve:

1. The issue of compiling the template at load-time and making sure it
   was installed in the theme package. The plugin would need to do
   this itself or the template would need to be included in 'core'.
   Thankfully, this should be easy with *cl-closure-template*.
2. More seriously, there is no formal relationship between content
   types and indexes. Consequentially, INDEXes include only POST
   objects at the moment. Whether the INDEX should specify what
   Content Types it includes or the CONTENT which indexes it appears
   on is not yet clear.

### Contributing

The preferred workflow is more or less:

    - fork and clone
    - make a branch
    - commit your changes
    - push your branch to your github fork
    - open a Pull Request

#### Fork and clone

You may clone the main github repository or your fork, whichever you cloned
will be known as origin in your git repository. You have to add the other git
repository to your remotes, so if you cloned from your fork execute:


```bash
git remote add upstream git@github.com:redline6561/coleslaw.git
```

If you cloned from the main github repository execute:

```bash
git remote add fork git@github.com:<YourUsername>/coleslaw.git
```

For the rest of the steps we will assume you cloned from your fork and that the main github repository has the remote name of upstream.

#### Make a branch

```bash
git checkout -b <branch_name>
```

It is important to work always on branch so one can track changes in upstream by simply executing ```git pull upstream master:master``` from the master branch. If one can't come up with a suitable branch name just name it patch-n.
2
#### Commit your changes

Make the changes you want to coleslaw, add the files with that changes (```git add <path/to/file>```) and commit them (```git commit```). Your commit message should strive to sum up what has changes and why.

#### Push your branch to your github fork

```bash
git push origin branch
```

#### Open a Pull Request

After pushing the branch to your fork, on github you should see a button to open a pull request. In the PR message give the rationale for your changes.

[closure_template]: https://github.com/archimag/cl-closure-template
[api_docs]: https://github.com/redline6561/coleslaw/blob/master/docs/plugin-api.md
[clmd]: https://github.com/gwkkwg/cl-markdown
[clrz]: https://github.com/redline6561/colorize
[pyg]: http://pygments.org/
[incf]: https://github.com/redline6561/coleslaw/blob/master/plugins/incremental.lisp
[asdf3]: https://github.com/fare/asdf3-2013
[qpq]: https://github.com/sellout/quid-pro-quo
[misu]: https://blogs.janestreet.com/effective-ml-revisited/
[wookie]: https://github.com/orthecreedence/wookie/blob/master/plugin.lisp#L181
