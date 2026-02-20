# Agent Guide for subsid.github.io

This repository is a personal blog/notes system that uses Emacs org-mode to publish org files to static HTML. It generates two versions: a public blog (`public/`) and a private blog (`private/`).

## Project Architecture

- **Source**: Org files located in `~/Dropbox/notes/org_roam_v2/pages/`
  - `article/` - Article content
  - `main/` - Main notes
  - `reference/` - Reference materials and snippets
- **Output**: 
  - `public/` - Public blog (published to GitHub Pages)
  - `private/` - Private blog (local only, excluded from git)
- **Build System**: Uses `orgroam-publish` generator (in `generator/` directory)
  - Core build logic: `generator/src/build-site-config.el`
  - Site customization: `site-config.el` (in repo root)
- **Static Assets**: `content/published/` contains CSS/JS files

## Build Commands

### Full Build
```bash
# Build both public and private blogs
./scripts/build.sh

# Or call the generator directly:
./generator/scripts/build.sh
```

### Incremental Build
```bash
# Watch for changes and rebuild incrementally
./scripts/build.sh watch

# Manually build a single file
./scripts/build.sh incremental /path/to/file.org
```

### Serve Locally
```bash
# Serve public blog on http://localhost:8000
./scripts/serve.sh

# Serve private blog on http://localhost:8000
./scripts/serve.sh private

# Custom port
./scripts/serve.sh public 3000
```

### Deploy
```bash
# Build and deploy to GitHub Pages
./scripts/deploy.sh

# With custom commit message
./scripts/deploy.sh "Your custom message"
```

## Code Style Guidelines

### Emacs Lisp

#### File Organization
- Configuration file: `generator/src/build-site-config.el` (main config, ~440 lines)
- Entry point: `generator/src/build-site.el` (minimal, loads config and runs build)
- Incremental build: `generator/src/incremental-build.el` (single-file publishing)
- Site customization: `site-config.el` (root of this repo, overrides generator defaults)

#### Naming Conventions
- **Functions**: Use `my-` or `my/` prefix for custom functions
  - Example: `my-html-publish-with-tags`, `my/org-html-src-block`
- **Variables**: Use descriptive names with hyphens
  - Example: `my-site-preamble`, `common-html-properties`, `org-publish-project-alist`
- **Private/helper functions**: Use descriptive names without special prefixes
  - Example: `get-file-tags-info`, `has-tag-p`, `create-sitemap-entry`

#### Function Documentation
- Always include docstrings for public functions
- Format: `"Brief description. Details about PARAMETERS in UPPERCASE."`
- Example:
  ```elisp
  (defun my-html-publish-with-tags (plist filename pub-dir &optional filter-fn)
    "Publish to HTML based on optional FILTER-FN predicate.
     If FILTER-FN is nil, publish all files.
     Remove title from export if 'notitle' tag is present."
    ...)
  ```

#### Code Organization Patterns
1. **Package setup first** - Initialize package system and install dependencies
2. **Variable definitions** - Define all `defvar` constants
3. **Helper functions** - Small utility functions
4. **Publishing functions** - Core publishing logic
5. **Project configuration** - `org-publish-project-alist` setup
6. **Advice and hooks** - Modifications to org-mode behavior

#### Publishing Tag System
Content is controlled via org-mode tags in file headers:
- `#+TAGS: publish` - Include in public blog
- `#+TAGS: sitemapignore` - Exclude from sitemap generation
- `#+TAGS: notitle` - Don't export title

#### Error Handling
- Use `when` for single-branch conditionals
- Use `let*` for sequential bindings where later vars depend on earlier ones
- Check file existence with `(file-regular-p file-path)` before processing
- Filter nil values from lists before processing: `(delq nil entries)`

#### Message/Logging
```elisp
;; Informational messages
(message "Exporting to blog %s" filename)

;; Suppress messages during batch operations
(let ((message-log-max nil)
      (inhibit-message t))
  ...)
```

### Shell Scripts

#### Shebang and Safety
```bash
#!/bin/bash

# For critical scripts (deploy.sh), use:
set -euo pipefail  # Exit on error, undefined vars, pipe failures
```

#### Conditionals
- Use clear `if-elif-else` structure for script modes
- Example from `build.sh`:
  ```bash
  if [ "$1" = "watch" ]; then
      # watch mode
  elif [ "$1" = "incremental" ]; then
      # incremental mode
  else
      # full build (default)
  fi
  ```

#### Variable Naming
- Use `UPPER_CASE` for environment/configuration variables
- Example: `CHANGED_FILE`, `BLOG_DIR`, `PORT`

### CSS

#### Structure
- Use CSS variables from simple.css framework (`var(--border)`)
- Organize by component:
  1. Site structure (header, nav)
  2. UI elements (avatar, buttons)
  3. Content-specific (related-zettels)

#### Naming
- Use kebab-case: `.site-header`, `.avatar-large`
- Be specific: `.site-nav a` rather than generic selectors

## Project-Specific Conventions

### Org-Publish Projects
The `org-publish-project-alist` defines multiple projects:
- `my-org-notes-articles` - Articles for public blog
- `my-org-notes-main` - Main notes for public blog
- `my-org-notes-sitemap` - Combined sitemap (notes.org)
- `my-org-references-content` - References (references.org)
- `my-private-*` - Private versions (publish ALL files)
- `my-org-static` / `my-private-blog-static` - Static assets

### Publishing Functions
- `my-conditional-html-publish` - Publishes only files with "publish" tag
- `my-publish-all-html` - Publishes all files (used for private blog)
- Both respect `notitle` and `sitemapignore` tags

### Directory Structure Rules
1. **Never commit** `private/` directory (in .gitignore)
2. **Static assets** go in `content/published/` (copied to both public/ and private/)
3. **Emacs packages** installed to `.packages/` (excluded from git)
4. **Source files** remain in external org-roam directory (not in this repo)

### Common Patterns

#### Adding a New Publishing Function
```elisp
(defun my-new-publish-function (plist filename pub-dir)
  "Brief description."
  (let ((file-info (get-file-tags-info filename)))
    (when (should-publish-logic file-info)
      (message "Exporting to blog %s" filename)
      (org-html-publish-to-html plist filename pub-dir))))
```

#### Adding a New Project
```elisp
("my-new-project"
 :recursive t
 :base-directory "~/path/to/source"
 :base-extension "org"
 :publishing-directory "./public"
 :publishing-function my-conditional-html-publish
 ,@common-html-properties  ; Use common properties
 :auto-sitemap t)
```

#### Modifying HTML Output
Use advice to transform org-mode HTML output:
```elisp
(defun my/transform-html (html)
  "Transform HTML output."
  (replace-regexp-in-string "pattern" "replacement" html))

(advice-add 'org-html-src-block :filter-return #'my/transform-html)
```

## Testing Changes

1. **Test full build**: `./scripts/build.sh` - Ensure no errors
2. **Test incremental build**: `./scripts/build.sh incremental ~/path/to/test-file.org`
3. **Verify output**: `./scripts/serve.sh` and check http://localhost:8000
4. **Test private blog**: `./scripts/serve.sh private`
5. **Check git status**: Ensure private/ not tracked

## Common Tasks for Agents

### Adding New Static Assets
1. Place files in `content/published/static/` (CSS) or `content/published/js/` (JS)
2. Reference in `my-html-head` variable in `site-config.el` or generator's default config
3. No rebuild needed - static files copied automatically

### Modifying Navigation
Edit `my-site-preamble` or `my-private-site-preamble` variables in `site-config.el`

### Changing Publishing Logic
1. For site-specific changes: Add overrides to `site-config.el`
2. For generator changes: Edit `generator/src/build-site-config.el`
3. Test with both full and incremental builds

### Modifying Generator Defaults
1. Edit files in `generator/` directory
2. Changes will affect all sites using the generator
3. Commit changes to the generator repository separately

### Debug Build Issues
- Clear cache: `rm -rf ~/.org-timestamps/`
- Force full rebuild (cache cleared in config): `./scripts/build.sh`
- Check individual file: `emacs --batch --eval "(require 'ox-html)" file.org -f org-html-export-to-html`

## Important Notes

- **Dependencies**: Requires Emacs with package system, entr (for watch mode), Python 3 (for serving)
- **Org-roam integration**: This project consumes org-roam files but doesn't manage them
- **Deployment**: Uses `git subtree` to push `public/` folder to `main` branch on GitHub
- **Cache handling**: `org-publish-cache` cleared on each full build
- **Incremental builds**: Must initialize cache for all projects before publishing single file
- **Generator**: The `generator/` directory contains the `orgroam-publish` static site generator (separate git repository)
