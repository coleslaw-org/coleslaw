# Themes

The theming support in coleslaw is very flexible and relatively easy
to use. However it does require some knowledge of HTML, CSS, and how
coleslaw processes content.

To understand how coleslaw processes a blog, a look at the [overview][ovr]
documentation may prove useful. This document will focus mainly on the
template engine and how you can influence the resulting HTML.

**NOTE**: Themes are not able to change the generated file names or the
generated file structure on disk. They can change the resulting HTML, nothing more.

## High-Level Overview

Themes are written using [Closure Templates][clt]. Those templates are
then compiled into functions that Lisp calls with the blog data to get
HTML. Since the Lisp code to use theme functions is already written,
your theme must follow a few rules.

Every theme **must** be in a folder under "themes/" named after the
theme. The theme's templates must start with a namespace declaration
like so: `{namespace coleslaw.theme.$MY-THEME-NAME}`.

A theme must have three templates which take *specific arguments*
(to be described later).
1. Base
2. Post
3. Index

## Two types of pages

Coleslaw generates two types of pages: `index` pages and `post` pages.
Every page other than those in the `posts/` directory is an `index`.
**Every** page uses the `base.tmpl` and fills in the content using
either the `post` or `index` templates.

*  `base.tmpl` This template generates the outer shell of the HTML.
   It keeps a consistent look and feel for all pages in the blog. The
   actual content (i.e., not header/footer/css) comes from other templates.

*  `index.tmpl` This template generates the content of the `index` pages.
   That is, any page with more than one content object, e.g. the homepage.

*  `post.tmpl` This templates generates content for the individual posts.
   Coleslaw already converts the content of the individual post to HTML
   by using markdown (or RST). So this template is **not** used to
   convert an individual post, merely to give it a standard layout.

Here's a visual example to make things clearer:
```
INDEX HTML FILES                    INDIVIDUAL POST HTML FILES
|-------------------------|         |-------------------------|
| base.tmpl               |         | base.tmpl               |
|                         |         |                         |
|  |-------------------|  |         | |------------------|    |
|  |  index.tmpl       |  |         | | post.tmpl        |    |
|  |                   |  |         | |                  |    |
|  |-------------------|  |         | |------------------|    |
|                         |         |                         |
|-------------------------|         |-------------------------|
```

## Note on Style Sheets (css)

If you only want to change the way the blog is styled, it is probably
simplest to either modify the existing default theme, `hyde`, or copy
it in entirety and then tweak only the CSS of your new theme. A large
amount of visual difference can be had with a minimum of (or no)
template hacking. There is plenty of advice on CSS styling on the web.
I'm no expert but feel free to send patches to hyde's `style.css` or a
recommended CSS resource for this guide.

## Creating a Theme from Scratch (with code)

### Step 1. Create the directory.

A theme name must be a valid lisp symbol. For this example, we'll use
`trivial`, so create a `themes/trivial` directory in the *coleslaw* repo.

### Step 2. Create the templates.

As described above, we need 3 template files `base.tmpl`, `post.tmpl`
and `index.tmpl`. Initially, let's just create the simplest theme that
compiles correctly.

base.tmpl:
```
{namespace coleslaw.theme.trivial}
{template base}
{/template}
```
post.tmpl:
```
{namespace coleslaw.theme.trivial}
{template post}
{/template}
```
index.tmpl:
```
{namespace coleslaw.theme.trivial}
{template index}
{/template}
```

This will create three template functions that coleslaw can find, named
`base`, `post`, and `index`.

### Step 3. Use it in your config.

At this point, you can change the `:theme` in your `.coleslawrc` to
`trivial` and then generate your blog with `(coleslaw:main)`. However,
all the HTML files will be empty because our templates are empty!

### Intermezzo I, The Templating Language

The templating language is documented [elsewhere][clt].
However as a short primer:

*  Everything is output literally, except template commands.
*  Template commands are enclosed in `{` and `}`
*  Variables, which are provided by coleslaw, can be referenced
   inside a template command. So to use a variable you have to say
   `{$variable}` or `{$variable.key}`.
*  If statements are written as `{if ...} ... {else} ... {/if}`.
   Typical examples are: `{if $injections.body} ... {/if}` or
   `{if not isLast($link)} ... {/if}`.
*  Loops can be written as `{foreach $var in $sequence} ... {/foreach}`.

### Intermezzo II, Variables provided by Coleslaw

The variable that is available in all templates is:
- **config**       This contains the `.coleslawrc` content.

#### Base Template Variables

- **raw**          HTML generated by a sub template, `index` or `post`.
- **content**      The object which was used to generate **raw**.
- **pubdate**      A string containing the publication date.
- **injections**   A list containing the injections. Injections are used
                   by plugins mostly to add Javascript to the page.

#### Index Template Variables

- **tags**         A list containing all the tags, each with keys
                   `.name` and `.slug`.
- **months**       A list of all months with posts as `yyyy-mm` strings.
- **index**        This is the meat of the content. This variable has
                   the following keys
 - `id`, the name of the page that will be rendered
 - `posts`, a list of posts (see below)
 - `title`, a string title to display to the user
- **prev**         If this index file is part of a chain, the `id`
                   of the previous index html in the chain.
                   If this is the first file, the value will be empty.
- **next**         If this index file is part of a chain, the `id`
                   of the next index html in the chain.
                   If this is the last file, the value will be empty.

#### Post Template Variable

- **prev**
- **next**
- **post**         All these variables are post objects. **prev** and
                   **next** are the adjacent posts when put in
                   chronological order. Each post has the following keys:
   - `tags`, a list of tags (each with keys `name` and `slug`)
   - `slug`, the slug of the post
   - `date`, the date of posting
   - `text`, the HTML of the post's body
   - `title`, the title of the post

### Step 4. Include the content

*NOTE*: We can keep the template engine from escaping raw HTML by
adding a `|noAutoescape` clause to commands, like so: `{$raw |noAutoescape}`.

Let's now rewrite `base.tmpl` like this:
```
{namespace coleslaw.theme.trivial}
{template base}
<html>
  <head><title>Trivial Theme For Coleslaw</title></head>
  <body>
    <h1>All my pages have this title</h1>
    {$raw |noAutoescape}
  </body>
</html>
{/template}
```

A simple `index.tmpl` looks like this:
```
{namespace coleslaw.theme.trivial}
{template index}
{foreach $post in $index.posts}
<h1>{$post.title}</h1>
  {$post.text |noAutoescape}
{/foreach}
{/template}
```

And a simple `post.tmpl` is similarly:
```
{namespace coleslaw.theme.trivial}
{template post}
<h1>{$post.title}</h1>
  {$post.text |noAutoescape}
{/template}
```

### Conclusion

All of the files are now populated with content. There are still no links
between the pages so navigation is cumbersome but adding links is simple.
Good luck!

## Note on adding links

As mentioned earlier, most files have a file name which is a slug of
some sort. So if you want to create a link to a tag file you should
do something like this: `<a href="${config.domain}/tags/{$tag.slug}">{$tag.name}</a>`.

[clt]: https://developers.google.com/closure/templates/
[ovr]: https://github.com/redline6561/coleslaw/blob/master/docs/overview.md
