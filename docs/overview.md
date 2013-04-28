## Overall Structure

Conceptually, coleslaw processes a blog as follows:

1.  Coleslaw loads the user's config, then reads a directory containing
    `.post` files and processes them into POST and INDEX objects.

2.  The POST and INDEX objects are then fed to the templating engine
    to produce HTML files.

3.  A deploy method is called (possibly customized with plugins) to make
    the resulting HTML files (and any static content) visible to the web.

## What files are generated anyway?

Before we dive into other details, it is useful to know the directory
structure of the generated content, so you know how the relative path
different content resides at.

The blog's toplevel looks like this:
```
index.html
posts/
date/
tag/
css/
static/ (optional)
```

### index.html

This file is the blog homepage, as you'd expect.  It contains a list of
the most recent posts and has links to the different archives.

### posts directory

This directory contains an `.html` file per post.  The name of the file
is the post's `slug`.

### date directory

This directory contains an `.html` file per month, for each month with
published content. The name of the file is of the form `yyyy-mm.html`.

### tag directory

This directory contains an `.html` file per tag, each containing all
posts with that tag. The name of the file is the tag's `slug`.

### css directory

This directory is a copy of the `css/` folder of the theme.

### static directory (optional)

This directory is a copy of the `static/` directory of the blog's git repo.
