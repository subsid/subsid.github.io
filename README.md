# subsid.github.io

Personal blog and notes, built with [Emacs org-mode](https://orgmode.org/) and [ox-publish](https://orgmode.org/manual/Publishing.html). Source files are [org-roam](https://www.orgroam.com/) notes; this repo contains only the build tooling that converts them to static HTML.

Live site: [subsid.github.io](https://subsid.github.io)

---

## How It Works

Org files from an external org-roam directory are published to two output targets:

- **`public/`** — the public blog, deployed to GitHub Pages. Only files tagged `publish` are included.
- **`private/`** — a full local preview including unpublished drafts. Never committed to git.

### Tag System

Tags are set in an org file's header with `#+TAGS:`. Three tags control publishing behaviour:

| Tag             | Effect                                                                 |
|-----------------|------------------------------------------------------------------------|
| `publish`       | Include the file in the public blog                                    |
| `sitemapignore` | Publish the HTML but exclude the file from the sitemap listing         |
| `notitle`       | Suppress the `<h1>` title in the exported HTML (useful for custom layouts) |

Example org file header:

```org
#+title: My Article
#+TAGS: publish
#+EXPORT_FILE_NAME: my-article
#+DATE: <2025-03-01 Sat>
```

### Source Directory Layout

The build expects org files organised into three subdirectories:

```
pages/
├── article/    # Long-form articles and posts
├── main/       # Short notes and zettels
└── reference/  # Reference material, snippets, link collections
```

Files in `project/`, `work/`, `Inbox.org`, and `Mobile Inbox.org` are always excluded.

### Sitemap Generation

Two sitemaps are generated automatically:

- **`notes.html`** — lists all published `article/` and `main/` files, sorted by date (newest first)
- **`references.html`** — lists all published `reference/` files, with the Links file pinned to the top

---

## Prerequisites

| Dependency | Purpose                          | Required?                  |
|------------|----------------------------------|----------------------------|
| Emacs 28+  | Build engine (ox-publish)        | Yes                        |
| Python 3   | Local preview server             | For `serve.sh` only        |
| `entr`     | File watching for incremental builds | For `build.sh watch` only |

Install `entr` on Debian/Ubuntu:

```bash
apt install entr
```

---

## Setup

To use this as the basis for your own org-roam blog:

**1. Clone the repo**

```bash
git clone https://github.com/subsid/subsid.github.io.git
cd subsid.github.io
```

**2. Point the build at your org files**

Open `src/build-site-config.el` and update the `:base-directory` in each project to point to your own org-roam pages directory. It appears in several places — search for `~/Dropbox/notes/org_roam_v2/pages` and replace all occurrences:

```elisp
:base-directory "~/path/to/your/org/pages"
```

**3. Tag your org files**

Add `#+TAGS: publish` to any org file you want to appear on the public blog. Use `#+EXPORT_FILE_NAME:` to set a clean output filename (otherwise the source filename is used):

```org
#+title: My First Post
#+TAGS: publish
#+EXPORT_FILE_NAME: my-first-post
#+DATE: <2025-01-01 Wed>

Content goes here.
```

**4. Run a build**

```bash
./scripts/build.sh
```

Output appears in `public/`. Open `public/index.html` in a browser, or use `serve.sh` for a proper local server.

---

## Scripts

### Build

```bash
# Full clean build (clears public/ and private/ before rebuilding)
./scripts/build.sh

# Watch for file changes and incrementally rebuild on save
./scripts/build.sh watch

# Rebuild a single file (used internally by watch mode)
./scripts/build.sh incremental /path/to/file.org
```

### Serve locally

```bash
# Serve public/ on http://localhost:8000
./scripts/serve.sh

# Serve private/ (includes unpublished drafts)
./scripts/serve.sh private

# Custom port
./scripts/serve.sh public 3000
```

### Deploy

```bash
# Build and push public/ to GitHub Pages
./scripts/deploy.sh

# With a custom commit message
./scripts/deploy.sh "update notes"
```

Initialises a throwaway git repo inside `public/`, commits all generated files, and force-pushes to `origin/main`. The `public/` directory is gitignored on the working branch — only the generated output lands on `main`.

### Test

```bash
./scripts/test.sh
```

---

## Project Structure

```
subsid.github.io/
├── src/
│   ├── build-site-config.el   # All project definitions, tag logic, sitemap functions
│   ├── build-site.el          # Full build entry point (loads config and runs org-publish)
│   └── incremental-build.el   # Single-file publish logic used by watch mode
├── scripts/
│   ├── build.sh               # Full / watch / incremental build
│   ├── deploy.sh              # Build and deploy to GitHub Pages
│   ├── serve.sh               # Local preview server
│   └── test.sh                # Run the test suite
├── content/
│   └── published/             # Static assets (CSS, JS) copied to public/ and private/
├── test/
│   └── fixtures/              # Mock org files used by the test suite
│       └── pages/
│           ├── article/
│           ├── main/
│           └── reference/
├── public/                    # Generated public site (committed via git subtree)
├── private/                   # Generated private site (local only, gitignored)
└── LICENSE
```

---

## Running Tests

Tests use Emacs's built-in [ERT](https://www.gnu.org/software/emacs/manual/html_node/ert/) framework and run against the fixture files in `test/fixtures/` — no real notes are required.

```bash
./scripts/test.sh
```

15 tests across three categories:

- **Tag tests** — `get-file-tags-info` correctly detects `publish`, `notitle`, and `sitemapignore`
- **Sitemap unit tests** — `create-sitemap-entry` path handling, `EXPORT_FILE_NAME` resolution, pinned entry ordering
- **Integration tests** — full `ox-publish` run verifying published files appear in the right place, unpublished files are excluded, `notitle` suppresses the heading, `sitemapignore` keeps files out of the sitemap, and the Links reference is pinned first

---

## License

The build tooling in this repository (scripts, Emacs Lisp source, test fixtures) is released under the [MIT License](LICENSE). This covers the software only — not the blog posts and notes, which remain personal content.

---

*Inspired by the [System Crafters org-website-example](https://github.com/SystemCrafters/org-website-example).*
