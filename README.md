Personal blog using org files and ox-publish.

The script does a few things
1. Exports my [org-roam](https://github.com/subsid/emacs.d/blob/master/config/my-org-roam.el) files in the articles/ directory as html
2. I have 2 types of exports
    1. Public - Things I want to share publicly
    2. Private - Separate folder that I don't commit (usually run locally)
3. Sitemap generation - Each file is either ignored, added to "articles" or "snippets" sitemap.

This is a great place to get started - [System Crafters Example](https://github.com/SystemCrafters/org-website-example/tree/main)

Publish Content

``` bash
./build.sh

# Watch for changes and republish
./build.sh watch
```

Serve with python http server
```
# serves public/ dir
./serve.sh

# serves private/ dir (for local use)
./serve.sh private
```

