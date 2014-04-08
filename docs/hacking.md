## Coleslaw: A Hacker's Guide

Here we'll provide an overview of key concepts and technical decisions
in *coleslaw* and a few suggestions about future directions. Please
keep in mind that *coleslaw* was written on a lark when 3 friends had
the idea to each complete their half-dreamed wordpress replacement in
a week. Though it has evolved considerably since it's inception, like
any software some mess remains.

## Core Concepts

### Data and Deployment

**Coleslaw** is pretty fundamentally tied to the idea of git as both a
backing data store and a deployment method (via `git push`). The
consequence is that you need a bare repo somewhere with a post-recieve
hook. That post-recieve hook
([example](https://github.com/redline6561/coleslaw/blob/master/examples/example.post-receive))
will checkout the repo to a **$TMPDIR** and call `(coleslaw:main $TMPDIR)`.

It is then coleslaw's job to load all of your content, your config and
templates, and render the content to disk. Deployment is done by
updating a symlink and the default install assumes your webserver will
be configured to serve from that symlink. However, there are plugins
for deploying to Heroku, S3, and Github Pages.

### Blogs vs Sites

**Coleslaw** is blogware. When I designed it, I only cared that it
could replace my server's wordpress install. As a result, the code is
still structured in terms of POSTs and INDEXes. Roughly speaking, a
POST is a blog entry and an INDEX is a collection of POSTs or other
content. An INDEX really only serves to group a set of content objects
on a page, it isn't content itself.

This isn't ideal if you're looking for a full-on static site
generator.  Content Types were added in 0.8 as a step towards making
*coleslaw* suitable for more use cases but still have some
limitations. Chiefly, the association between Content Types, their
template, and their inclusion in an INDEX is presently ad-hoc.

### Current Content Types & Indexes

There are 3 INDEX subclasses at present: TAG-INDEX, DATE-INDEX, and
NUMERIC-INDEX, for grouping content by tags, publishing date, and
reverse chronological order, respectively. Currently, there is only 1
content type: POST, for blog entries.

I'm planning to add a content type PAGE, for static pages. It should
be a pretty straightforward subclass of CONTENT with the necessary
methods: `render`, `page-url` and `publish`, but will require a small
tweak to prevent showing up in any INDEX.

### Templates and Theming

User configs are allowed to specify a theme, otherwise the default is
used. A theme consists of a directory under "themes/" containing css,
images, and at least 3 templates: Base, Index, and Post.

**Coleslaw** exclusively uses
[cl-closure-template](https://github.com/archimag/cl-closure-template)
for templating which is a well documented CL implementation of
Google's Closure Templates. Each template file should be in a
namespace like `coleslaw.theme.theme-name`.

Each template creates a lisp function in the theme's package when
loaded. These functions take a property list (or plist) as an argument
and return rendered HTML.  **Coleslaw** defines a helper called
`theme-fn` for easy access to the template functions.

### The Lifecycle of a Page

- `(load-content)`

A page starts, obviously, with a file. When
*coleslaw* loads your content, it iterates over a list of content
types (i.e. subclasses of CONTENT).  For each content type, it
iterates over all files in the repo with a matching extension,
e.g. ".post" for POSTs. Objects of the appropriate class are created
from each matching file and inserted into the `*content*` hash-table.

- `(compile-blog dir)`

Compilation starts by ensuring the staging directory (`/tmp/coleslaw/`
by default) exists, cd'ing there, and copying over any necessary theme
assets. Then *coleslaw* iterates over the content types, calling the
`publish` method on each one. Publish creates any non-INDEX pages for
the objects of that content type by iterating over the objects in an
appropriate fashion, rendering them, and passing the result to
`write-page` (which should probably just be renamed to `write-file`).

After this, `render-indexes` and `render-feeds` are called, and an
'index.html' symlink is created to point to the first reverse
chronological index.

- `(deploy dir)`

Finally, we move the staging directory to a timestamped path under the
the config's `:deploy-dir`, delete the directory pointed to by the old
'.prev' symlink, point '.curr' at '.prev', and point '.curr' at our
freshly built site.

## Areas for Improvement

### Better Content Types

Creating a new content type should be both straightforward and doable
as a plugin. All that is really required is a subclass of CONTENT with
any needed slots, a template, a `render` method to call the template
with any needed options, a `page-url` method for layout, and a
`publish` method.

Unfortunately, this does not solve:

1. The issue of compiling the template at load-time and making sure it
   was installed in the theme package. The plugin would need to do
   this itself or the template would need to be included in 'core'.
2. More seriously, there is no formal relationship between content
   types and indexes. Indices include *ALL* objects in the `*content*`
   hash table. This may be undesirable and doesn't permit indexes
   dedicated to particular content types.

### New Content Type: Shouts!

I've also toyed with the idea of a content type called a SHOUT, which
would be used primarily to reference or embed other content, sort of a
mix between a retweet and a del.icio.us bookmark. We encounter plenty
of great things on the web. Most of mine winds up forgotten in browser
tabs or stored on twitter's servers. It would be cool to see SHOUTs as
a plugin, probably with a dedicated SHOUT-INDEX, and some sort of
oEmbed/embed.ly/noembed support.

### Layouts and Paths

Defining a page-url for every content-object and index seems a bit
silly. It also spreads information about the site layout throughout
the codebase, it might be better to have a slot in the config that
defines this information with a key to go with each format string.
Adding a new content-type as a plugin could then provide a default
by banging on the config or specify the path in its `enable` options.

### Incremental Compilation

Incremental compilation is doable, even straightforward if you ignore
indexes. It is also preferable to building the site in parallel as
avoiding work is better than using more workers. Moreover, being
able to determine (and expose) what files just changed enables new
functionality such as plugins that cross-post to tumblr.

Git's post-receieve hook is supposed to get a list of refs on $STDIN.
A brave soul could update our post-receive script to figure out the
original hash and pass that along to `coleslaw:main`. We could then
use it to run `git diff --name-status $HASH HEAD` to find changed
files and act accordingly.

This is a cool project and the effects are far reaching. Among other
things the existing deployment model would not work as it involves
rebuilding the entire site. In all likelihood we would want to update
the site 'in-place'. Atomicity of filesystem operations would be a
reasonable concern. Also, every numbered INDEX would have to be
regenerated along with any tag or month indexes matching the
modified files. If incremental compilation is a goal, simply
disabling the indexes may be appropriate for certain users.
