#!/bin/bash

set -euo pipefail

echo "Building site"
# Build site
./build.sh

echo "Committing changes"
# Commit changes
git add -A
msg="rebuilding site $(date)"
if [ -n "$*" ]; then
  msg="$*"
fi
git commit -m "$msg"

echo "Deploying to main"
# Deploy to main
git push origin `git subtree split --prefix public org-hugo-site`:main --force
