# simple-wiki

A minimalist wiki written in haskell.

### What it does
simple-wiki is designed to mirror the functionality of github's wiki.

It serves `markdown` files, converted into `html`.

The file `README.markdown` would therefore correspond to
`localhost:8000/README`.

### Installation
This package installs like any other haskell package:
```bash
$ runhaskell Setup.hs configure
$ runhaskell Setup.hs build
$ runhaskell Setup.hs install
```

### Usage
To start it, simply run:
```
$ simple-wiki path/to/wiki/root
```

### Configuration
To configure simple-wiki, create a file named `simple-wiki.json` in the root
of your wiki directory.

You can configure the following:
- `"style"`: The path to a CSS stylesheet within the directory (default: no stylesheet)
- `"port"`: The port the wiki is served on (default: `8000`)
- `"index"`: The name of the index markdown file of your wiki (default: `"index"`)

### Dependencies
simple-wiki uses:
- [happstack] for its web functionality
- [blaze-html] for the website templating
- [pandoc] for the markdown to html conversion

[happstack]: http://happstack.com/page/view-page-slug/1/happstack
[blaze-html]: http://jaspervdj.be/blaze/
[pandoc]: http://johnmacfarlane.net/pandoc/
