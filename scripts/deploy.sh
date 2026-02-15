#!/bin/bash

set -euo pipefail

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
REMOTE=$(git -C "$REPO_DIR" remote get-url origin)

msg="rebuilding site $(date)"
if [ -n "${*:-}" ]; then
  msg="$*"
fi

echo "Building site..."
cd "$REPO_DIR"
./scripts/build.sh

echo "Deploying public/ to origin/main..."
cd "$REPO_DIR/public"
git init -q
git add -A
git commit -q -m "$msg"
git push -q "$REMOTE" HEAD:main --force
rm -rf "$REPO_DIR/public/.git"

echo "Deploy complete!"
