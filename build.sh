#!/bin/bash

if [ "$1" = "watch" ]; then
    echo "Watching for changes in content/..."
    find content/ \( -name "*.org" -o -name "*.css" \) -not -name "archive.org" | entr -r ./build.sh
else
    echo "Building site..."
    emacs -Q --script build-site.el
    echo "Build complete!"
fi
