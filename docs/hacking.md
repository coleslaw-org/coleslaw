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
[pygments][pyg] as a replacement.

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

There are also 4 helper functions provided that should prove useful in
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

### Incremental Compilation

Incremental compilation is doable, even straightforward if you ignore
indexes. It is also preferable to building the site in parallel as
avoiding work is better than using more workers. Moreover, being
able to determine (and expose) what files just changed enables new
functionality such as plugins that cross-post to tumblr.

This is a cool project and the effects are far reaching. Among other
things the existing deployment model would not work as it involves
rebuilding the entire site. In all likelihood we would want to update
the site 'in-place'. How to replace the compilation and deployment
model via a plugin has not yet been explored. Atomicity of filesystem
operations would be a reasonable concern. Also, every numbered INDEX
would have to be regenerated along with any tag or month indexes
matching the modified files. If incremental compilation is a goal,
simply disabling the indexes may be appropriate for certain users.

[post_receive_hook]: https://github.com/redline6561/coleslaw/blob/master/examples/example.post-receive
[closure_template]: https://github.com/archimag/cl-closure-template
[api_docs]: https://github.com/redline6561/coleslaw/blob/master/docs/plugin-api.md
[clmd]: https://github.com/gwkkwg/cl-markdown
[clrz]: https://github.com/redline6561/colorize
[pyg]: http://pygments.org/
