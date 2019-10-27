# Deploying on a standalone server

Coleslaw can deploy to a standalone server.
If you want this server installation, initialize a bare git repo and
set up the post-receive hook on that repo.

* First initialize a [git bare repo](http://git-scm.com/book/en/Git-on-the-Server-Setting-Up-the-Server) on the server.
* Copy [example post-receive hook][post_hook] to your blog's bare repo and set the executable bit (`chmod +x`).

* Point the web server at `:deploy-dir` attribute on the config file.
  Or "deploy-dir/.curr" if the `versioned` plugin is enabled.

[post_hook]: https://github.com/redline6561/coleslaw/blob/master/examples/example.post-receive
