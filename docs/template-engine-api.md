# Concepts

* **Theme Function**: A function to be called for rendering content. A Theme
  can provides multiple theme functions with different names. At a minimum 3
  functions must be provided, `'base`, `'index` and `'post`.

# Theme engine protocol

- `get-theme-fn`: Retrieves the theme function by name from the template engine.
- `compile-theme`: For themes need to be processed before used, **Coleslaw**
  provides this generic function. It is guaranteed to be called before calling
  `get-theme-fn`.
- `template-engine`: Retrieves the current theme engine from a `blog`
  instance. Mostly called with `*config*`, where the current `blog` instance,
  as its argument.

To implement a new template engine, one must define a method for at least
`get-theme-fn` that specializes on the template engine's name and set the
`template-engine` slot of the blog instance (in `*config*`) to the
corresponding name on the `enable` function of your plugin.
