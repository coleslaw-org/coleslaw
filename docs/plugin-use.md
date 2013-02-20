# General Use

* Add a list with the plugin name and settings to the ```:plugins``` section of your [.coleslawrc](http://github.com/redline6561/coleslaw/blob/master/examples/single-site.coleslawrc). Plugin settings are described below.

* Available plugins are listed below with usage descriptions and config examples.

## Analytics via Google

**Description**: Provides traffic analysis through [Google Analytics](http://www.google.com/analytics/).

**Example**: ```(analytics :tracking-code "google-provided-unique-id")```

## Comments via Disqus

**Description**: Provides comment support through [Disqus](http://www.disqus.com/).

**Example**: ```(disqus :shortname "disqus-provided-unique-id")```

## LaTeX via Mathjax

**Description**: Provides LaTeX support through [Mathjax](http://www.mathjax.org/) for posts tagged with "math" and indexes containing such posts. Any text enclosed in $$ will be rendered, for example, ```$$ \lambda \scriptstyle{f}. (\lambda x. (\scriptstyle{f} (x x)) \lambda x. (\scriptstyle{f} (x x))) $$```.

**Example**: ```(mathjax)```

## ReStructuredText

**Description**: Some people really like [ReStructuredText](http://docutils.sourceforge.net/rst.html). Who knows why? But it only took one method to add, so yeah! Just create a post with ```format: rst``` and the plugin will do the rest.

**Example**: ```(rst)```

## S3 Hosting

**Description**: Allows hosting your blog entirely via [Amazon S3](http://aws.amazon.com/s3/). It is suggested you closely follow the relevant [AWS guide](http://docs.aws.amazon.com/AmazonS3/latest/dev/website-hosting-custom-domain-walkthrough.html) to get the DNS setup correctly. Your ```:auth-file``` should match that described in the [ZS3 docs](http://www.xach.com/lisp/zs3/#file-credentials).

**Example**: ```(s3 :auth-file "/home/redline/.aws_creds" :bucket "blog.redlinernotes.com")```

## Wordpress Importer

**Description**: Import blog posts from Wordpress using their export tool. Blog entries will be read from the XML and converted into .post files. Afterwards the XML file will be deleted to prevent reimporting. Optionally an ```:output``` argument may be supplied to the plugin. If provided, it should be a directory in which to store the .post files. Otherwise, the value of ```:repo``` in your .coleslawrc will be used.

**Example**: ```(import :filepath "/home/redline/redlinernotes-export.timestamp.xml" :output "/home/redlinernotes/blog/")```
