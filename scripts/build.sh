#!/bin/bash

if [ "$1" = "watch" ]; then
    echo "Watching for changes in ~/Dropbox/notes/org_roam_v2/pages/{article,main,reference}/..."
    find ~/Dropbox/notes/org_roam_v2/pages/{article,main,reference} -name "*.org" \
        -not -name "notes.org" -not -name "references.org" -not -name "private.org" -not -name "sitemap.org" | \
        entr ./scripts/build.sh incremental /_
elif [ "$1" = "incremental" ]; then
    CHANGED_FILE="$2"
    if [ -n "$CHANGED_FILE" ]; then
        echo "Incrementally building changed file: $CHANGED_FILE"
        emacs -Q --batch --eval "(setq changed-file \"$CHANGED_FILE\")" -l ./src/incremental-build.el
        echo "Incremental build complete!"
    else
        echo "No file specified for incremental build"
    fi
else
    echo "Building site (full rebuild)..."
    emacs -Q --script ./src/build-site.el
    echo "Build complete!"
fi
