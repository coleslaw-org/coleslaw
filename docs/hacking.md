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

### Data and Deployment

**Coleslaw** is pretty fundamentally tied to the idea of git as both a
backing data store and a deployment method (via `git push`). The
consequence is that you need a bare repo somewhere with a post-recieve
hook. That post-recieve hook ([example][post_receive_hook])
will checkout the repo to a **$TMPDIR** and call `(coleslaw:main $TMPDIR)`.

It is then coleslaw's job to load all of your content, your config and
templates, and render the content to disk. Deployment is done by
moving the files to a location specified in the config and updating a
symlink.  It is assumed a web server is set up to serve from that
symlink. However, there are plugins for deploying to Heroku, S3, and
Github Pages.

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

### The Lifecycle of a Page

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

Finally, we move the staging directory to a timestamped path under the
the config's `:deploy-dir`, delete the directory pointed to by the old
'.prev' symlink, point '.curr' at '.prev', and point '.curr' at our
freshly built site.

### Blogs vs Sites

**Coleslaw** is blogware. When I designed it, I only cared that it
could replace my server's wordpress install. As a result, the code
until very recently was structured in terms of POSTs and
INDEXes. Roughly speaking, a POST is a blog entry and an INDEX is a
collection of POSTs or other content. An INDEX really only serves to
group a set of content objects on a page, it isn't content itself.

This isn't ideal if you're looking for a full-on static site
generator.  Thankfully, Content Types were added in 0.8 as a step
towards making *coleslaw* suitable for more use cases. Any subclass of
CONTENT that implements the *document protocol* counts as a content
type. However, only POSTs are currently included in the basic INDEXes
since there isn't yet a formal relationship to determine which content
types should be included on which indexes.  Users may easily implement
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

- `page-url`: Generate a relative path for the object on the site,
  usually sans file extension. If there is no extension, an :around
  method adds "html" later. The `slug` slot on the instance is
  conventionally used to hold a portion of the path that corresponds
  to a unique Primary Key or Object ID.

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

## Areas for Improvement

### Unified Routing

Right now, the templates break if you use a custom routing scheme
in your config. This is flatly ridiculous. The templates should be
updated to use a url field stored on each object which will store
the path of the object. This can be done transparently to users,
though must be handled with care and may involve refactoring
in the document protocol. Test carefully. If we cheat by hardcoding
the sitemap and/or feeds, that's probably okay for the moment.

Turns out this is even messier than I thought. Links are built from
scratch in the templates *all over the place*. Tags in posts,
taglinks and monthlinks in indexes, and prev/next links in numeric
indexes. I'm also doing two `find-all` calls in the base `render`
method for index. So I should profile and/or memoize that if needed.

We should also not have a **slug** _and_ **path** slot due to this.
Currently, we track uniqueness in the site by using a hashtable
keyed on `page-url`, i.e. relative paths. The paths are mostly
constructed from slugs. Sometimes we use the slugs for sorting
or other purposes. We should be able to build the paths at instance
creation time and scrap the slugs altogether. Note that this will
require changes to how we sort `NUMERIC-INDEX`.

### Immutable Data

Currently, most of our content classes (and the config class) have
*accessor* slots in addition to *reader* slots. There should be no
need for accessors as our data doesn't change after
initialization. They are computed from the git repo, dumped into
templates, and that's it. This is really a question of how we
initialize things. I settled on the current `load-content` scheme,
flawed as it is, because it is simple and lightweight.

Content files in the git repo have a file extension denoting the
content type, a header section with metadata and a text body. That
gets transformed into a `construct` call (and ultimately
`make-instance`) to the class matching the file extension, and a bunch
of initargs. We avoid code duplication for lots of unique
constructors, but we lose out some in error handling, separation of
interface and implementation, etc.

Keene's Object Oriented Programming in CL has good advice on this
situation, specifically on page 162, Separation of Initargs and Slot Names.

### Finish up Basic Deploy

The deploy method has been simplified and a git-hook plugin added.
Documentation and testing for the other deploy plugins is next.

### Deprecate :repo config option?

Coleslaw must at this point *always* be called with `coleslaw:main`
passing the repo directory as an argument. Given this, there is no
need for users to specify a repo in their config.

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
2. Custom themes that try to access non-existent properties of content
   do not currently error. They just wind up returning whitespace.
   When the theme compiles, we should alert the user to any obvious
   issues with it.
3. Trying to load content from the bare repo instead of the clone.
   i.e. Specifying the `:repo` in .coleslawrc as the bare repo.
   The README should clarify this point and the need for posts to be
   ".post" files.

### Scripting Conveniences/Basic Install

Right now, we assume that you want to set up a bare repo and use git push
for site deploys. However good a system this may be, we don't need to have
it as a baked in assumption for coleslaw. The current deploy system should
be moved into a plugin and replaced with something braindead
(e.g. `mv` staging-dir to deploy-dir).

This leads to the following:

1. Simplify the getting started process for new users.
   They should just be able to do
   `(progn
      (ql:quickload :coleslaw)
      (coleslaw:main "/path/to/my/blog-repo"))`.
   We'll still require a git repo for now. This does necessitate updating
   `get-updated-files` for the case where no revision is passed.
2. We could also add command-line tools/scripts to run coleslaw, set up
   the db for incremental builds, scaffold a new post, etc. for new users.
   Xach's buildapp or cl-launch would be useful here. frog and hakyll are
   good points of inspiration as well.

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

[post_receive_hook]: https://github.com/redline6561/coleslaw/blob/master/examples/example.post-receive
[closure_template]: https://github.com/archimag/cl-closure-template
[api_docs]: https://github.com/redline6561/coleslaw/blob/master/docs/plugin-api.md
[clmd]: https://github.com/gwkkwg/cl-markdown
[clrz]: https://github.com/redline6561/colorize
[pyg]: http://pygments.org/
[incf]: https://github.com/redline6561/coleslaw/blob/master/plugins/incremental.lisp
[asdf3]: https://github.com/fare/asdf3-2013
[qpq]: https://github.com/sellout/quid-pro-quo
[misu]: https://blogs.janestreet.com/effective-ml-revisited/
