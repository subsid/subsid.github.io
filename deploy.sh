#!/bin/bash

set -euo pipefail

echo "Building site"
# Build site
hugo

echo "Deploying to main"
# Deploy to main
git push origin `git subtree split --prefix public org-hugo-site`:main --force

