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
* `:lang`          => to set HTML attributes indicating the site language, default: "en"
* `:license`       => to override the displayed content license, the default is CC-BY-SA
* `:page-ext`      => to set the suffix of generated files, default: "html". "" works
  too (no extension). Does not effect `:index-ext`.
* `:index-ext`     => The extension for "index.EXT", default "html".
* `:plugins`       => to configure and enable coleslaw's [various plugins][plugin-use]
* `:separator`     => to set the separator for content metadata, default: ";;;;;"
* `:sitenav`       => to provide relevant links and ease navigation
* `:staging-dir`   => for Coleslaw to do intermediate work, default: "/tmp/coleslaw"
* `:rsync-passfile`=> The directory to a password file for use in rsync (via
  sshpass). Must also set `:which-sshpass`.
* `:which-sshpass` => The location of the `sshpass` program (try calling
 `$ which sshpass` to get it). Should also set `:rsync-passfile` if you are going 
 to use it.
* `:name-fn` => Define a function to call on the string title of posts for
  generating the URL. Defaults to `identity`, but it might make more sense to
  use `string-downcase`.


[Themes](https://github.com/redline6561/coleslaw/blob/master/docs/plugin-use.md)
 can support images in the navigation toolbar using `:image` as in the
example: `:sitenav ((:url "/index.html" :image "/img/home.png"))`. With the
`$link.image` variable.

[plugin-use]: https://github.com/redline6561/coleslaw/blob/master/docs/plugin-use.md
