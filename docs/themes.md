# Themes

The theming support in coleslaw is very flexible and relatively easy
to use. However it does require some knowledge of HTML, CSS, and how
coleslaw processes content.

To understand how coleslaw works, a look at the [hacking][hck]
documentation will prove useful. This document focuses mainly on the
template engine and how you can influence the resulting HTML.

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
either the `post` or `index` templates. No important logic should be
in *any* template, they are only used to provide a consistent layout.

*  `base.tmpl` This template generates the outer shell of the HTML.
   It keeps a consistent look and feel for all pages in the blog. The
   actual content (i.e., not header/footer/css) comes from other templates.

*  `index.tmpl` This template generates the content of the `index` pages.
   That is, any page with more than one content object, e.g. the homepage.

*  `post.tmpl` This templates generates content for the individual posts.

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
I'm no expert but feel free to send pull requests modifying a theme's
CSS or improving this section, perhaps by recommending a CSS resource.

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
*  Template commands are enclosed in `{` and `}`.
*  Variables, which are provided by coleslaw, can be referenced
   inside a template command. So to use a variable you have to say
   `{$variable}` or `{$variable.key}`.
   **WARNING**: At present, cl-closure-template does not have great debugging.
   If you typo this, e.g. `${variable}`, you will receive an *uninformative*
   and apparently unrelated error. Also, attempted access of non-existent keys
   fails silently. We are exploring options for making debugging easier in a
   future release.
*  If statements are written as `{if ...} ... {else} ... {/if}`.
   Typical examples are: `{if $injections.body} ... {/if}` or
   `{if not isLast($link)} ... {/if}`.
*  Loops can be written as `{foreach $var in $sequence} ... {/foreach}`.

### Intermezzo II, Variables provided by Coleslaw

The variable that should be available to all templates is:
- **config**       This contains the `.coleslawrc` content.

#### Base Template Variables

- **raw**          HTML generated by a sub template, `index` or `post`.
- **content**      The object which was used to generate **raw**.
- **pubdate**      A string containing the publication date.
- **injections**   A list containing the injections. Injections are used
                   by plugins mostly to add Javascript to the page.

#### Index Template Variables

- **tags**         A list containing all the tags, each with keys
                   `name` and `url`.
- **months**       A list of all the content months, each with keys
                   `name` and `url`.
- **index**        This is the meat of the content. This variable has
                   the following keys:
   - `content`, a list of content (see below)
   - `name`,  a name to use in links or href tags
   - `title`, a title to use in H1 or header tags
- **prev**         Nil or the previous index with keys: `url` and `title`.
- **next**         Nil or the next index with keys: `url` and `title`.

#### Post Template Variable

- **prev**
- **next**
- **post**         All these variables are post objects. **prev** and
                   **next** are the adjacent posts when put in
                   chronological order. Each post has the following keys:
   - `url`, the relative url of the post
   - `tags`, a list of tags (each with keys `name` and `url`)
   - `date`, the date of posting
   - `text`, the HTML of the post's body
   - `title`, the title of the post
   - `excerpt`, the excerpt of the post, same as `text` by default

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
{foreach $obj in $index.content}
<h1>{$object.title}</h1>
  {$object.excerpt |noAutoescape}
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
Just do: `<a href="{$config.domain}/{$object.url}">{$object.name}</a>`.

[clt]: https://developers.google.com/closure/templates/
[ovr]: https://github.com/redline6561/coleslaw/blob/master/docs/overview.md
[hck]: https://github.com/redline6561/coleslaw/blob/master/docs/hacking.md
