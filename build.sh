#!/bin/bash

if [ "$1" = "watch" ]; then
    echo "Watching for changes in ~/Dropbox/notes/org_roam_v2/pages/article/..."
    find ~/Dropbox/notes/org_roam_v2/pages/article/ \( -name "*.org" -o -name "*.css" \) -not -name "articles.org" -not -name "snippets.org" | entr -r ./build.sh
else
    echo "Building site..."
    emacs -Q --script build-site.el
    echo "Build complete!"
fi
