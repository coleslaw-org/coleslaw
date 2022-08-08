# Configuration

## Where

Coleslaw needs a `.coleslawrc` file to operate properly. That file is usually located at
$HOME/.coleslawrc but may also be placed in the blog repo itself.

## What

The only *required* information in the config is:
* `:author`       => to be placed on post pages and in the copyright/CC-BY-SA notice
* `:deploy-dir`   => for Coleslaw's generated HTML to go in
* `:domain`       => to generate absolute links to the site content
* `:routing`      => to determine the URL scheme of content on the site
* `:title`        => to provide a site title
* `:theme`        => to select one of the themes in "coleslaw/themes/"

It is usually recommend to start from the [example config][ex_config] and pare down from there.

[ex_config]: https://github.com/redline6561/coleslaw/blob/master/examples/example.coleslawrc

## Extras

There are also many *optional* config parameters such as:
* `:charset`       => to set HTML attributes for international characters, default: "UTF-8"
* `:feeds`         => to generate RSS and Atom feeds for certain tagged content
* `:excerpt-sep`   => to set the separator for excerpt in content, default: `<!--more-->`
* `:index-ext`     => The extension for the index, default "html" for index.html
* `:lang`          => to set HTML attributes indicating the site language, default: "en"
* `:license`       => to override the displayed content license, the default is CC-BY-SA
* `:name-fn`       => to modify URL strings after they are generated, default: `'identity`
* `:page-ext`      => to set the suffix of generated files, default: "html". "" for no extension
* `:plugins`       => to configure and enable coleslaw's [various plugins][plugin-use]
* `:separator`     => to set the separator for content metadata, default: ";;;;;"
* `:sitenav`       => to provide relevant links and ease navigation
* `:staging-dir`   => for Coleslaw to do intermediate work, default: "/tmp/coleslaw"

[plugin-use]: https://github.com/redline6561/coleslaw/blob/master/docs/plugin-use.md
