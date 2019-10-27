
# The Content Format

Coleslaw expects content to have a file extension matching the class
of the content. (I.e. `.post` for blog posts, `.page` for static pages, etc.)

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
