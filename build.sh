#!/bin/bash

if [ "$1" = "watch" ]; then
    echo "Watching for changes in ~/Dropbox/notes/org_roam_v2/pages/article/..."
    find ~/Dropbox/notes/org_roam_v2/pages/{article,main,reference} -name "*.org" \
    -not -name "notes.org" -not -name "references.org" -not -name "private.org" | \
    entr -r ./build.sh
else
    echo "Building site..."
    emacs -Q --script v2.el
    echo "Build complete!"
fi
