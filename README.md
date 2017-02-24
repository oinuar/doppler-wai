# Doppler Wai

Doppler Wai implements a HTML response for Wai server. This allows you to
write typesafe HTML inside your Wai server to implement standalone and fast
web servers.

## How to build?

Project depends on `doppler-html`, `doppler-css` and `doppler-event`.
The main project (`doppler-wai`) contains only Wai depedency sources.

To build the project, we use Cabal sandboxes. First, pull and configure
sandboxes for dependency projects.

```
cd {doppler-html, doppler-css, doppler-event}
cabal sandbox init --sandbox ../.doppler-sandbox
```

Now we can create and configure a sandbox for the main project.

```
cd doppler-wai
cabal sandbox init
cabal sandbox add-source ../doppler-html
cabal sandbox add-source ../doppler-css
cabal sandbox add-source ../doppler-event
cabal install --only-dep
cabal configure
```

You can then build the main project.

```
cabal build
```

This will build the library.
