# subsid.github.io

Personal blog and notes, built with [Emacs org-mode](https://orgmode.org/) and [ox-publish](https://orgmode.org/manual/Publishing.html). Source files are [org-roam](https://www.orgroam.com/) notes published using [orgroam-publish](https://github.com/subsid/orgroam-publish), a static site generator for org-roam.

Live site: [subsid.github.io](https://subsid.github.io)

---

## Features

- **Dual Blog System** — Generate both public (`public/`) and private (`private/`) versions
- **Tag-based Publishing** — Control what gets published with simple org-mode tags
- **Incremental Builds** — Fast rebuilds when editing individual files
- **Watch Mode** — Automatically rebuild on file changes
- **Sitemap Generation** — Automatic index pages sorted chronologically

---

## Quick Start

**1. Clone with submodules**

```bash
git clone --recursive https://github.com/subsid/subsid.github.io.git
cd subsid.github.io
```

**2. Build the site**

```bash
./scripts/build.sh
./scripts/serve.sh   # open http://localhost:8000
```

**3. Deploy**

```bash
./scripts/deploy.sh
```

---

## How It Works

This repository uses [orgroam-publish](https://github.com/subsid/orgroam-publish) (in `generator/`) to convert org-roam notes to static HTML. The generator is configured via `site-config.el` in this repo.

### Directory Structure

```
subsid.github.io/
├── generator/              # orgroam-publish (git submodule)
├── site-config.el         # Site-specific configuration
├── scripts/
│   ├── build.sh           # Wrapper calling generator/scripts/build.sh
│   ├── serve.sh           # Wrapper calling generator/scripts/serve.sh
│   └── deploy.sh          # Deploy to GitHub Pages
├── content/
│   └── published/         # Static assets (CSS, JS, images)
├── public/                # Generated public blog (deployed to main branch)
└── private/               # Generated private blog (gitignored)
```

### Configuration

The `site-config.el` file contains site-specific settings:

```elisp
;; Point to your org-roam directory
(setq my-org-pages-dir 
  (expand-file-name "~/Dropbox/notes/org_roam_v2/pages"))

;; Customize navigation
(setq my-site-preamble
  "<div class=\"site-header\">
  <nav class=\"site-nav\">
    <a href=\"/about.html\">About</a>
    <a href=\"/notes.html\">Notes</a>
    <a href=\"/references.html\">References</a>
  </nav>
</div>")
```

See `generator/site-config.example.el` for all available options.

### Publishing Tags

Tags are set in an org file's header with `#+TAGS:`:

| Tag             | Effect                                                                 |
|-----------------|------------------------------------------------------------------------|
| `publish`       | Include the file in the public blog                                    |
| `sitemapignore` | Publish the HTML but exclude from sitemap                             |
| `notitle`       | Suppress the `<h1>` title in the exported HTML                        |

Example:

```org
#+TITLE: My Article
#+TAGS: publish
#+EXPORT_FILE_NAME: my-article
#+DATE: <2025-03-01 Sat>

Content goes here.
```

### Source Directory Layout

The generator expects org files in three subdirectories:

```
pages/
├── article/    # Long-form articles and posts
├── main/       # Short notes and zettels
└── reference/  # Reference material, snippets, links
```

Files in `project/`, `work/`, `Inbox.org`, and `Mobile Inbox.org` are automatically excluded.

---

## Scripts

### Build

```bash
# Full clean build
./scripts/build.sh

# Watch for changes and rebuild incrementally
./scripts/build.sh watch

# Rebuild a single file
./scripts/build.sh incremental /path/to/file.org
```

### Serve Locally

```bash
# Serve public blog on http://localhost:8000
./scripts/serve.sh

# Serve private blog (includes unpublished drafts)
./scripts/serve.sh private

# Custom port
./scripts/serve.sh public 3000
```

### Deploy

```bash
# Build and push public/ to GitHub Pages
./scripts/deploy.sh

# With custom commit message
./scripts/deploy.sh "update notes"
```

The deploy script:
1. Runs a full build
2. Creates a temporary git repo in `public/`
3. Commits all generated files
4. Force-pushes to `origin/main`

---

## Prerequisites

| Dependency | Purpose                          | Required?                  |
|------------|----------------------------------|----------------------------|
| Emacs 28+  | Build engine (ox-publish)        | Yes                        |
| Python 3   | Local preview server             | For `serve.sh` only        |
| `entr`     | File watching                    | For `build.sh watch` only  |

Install `entr` on Debian/Ubuntu:

```bash
apt install entr
```

---

## Customization

### CSS

Add custom styles to `content/published/static/css/custom.css`. This file is copied to both `public/` and `private/` during builds.

### JavaScript

Place JS files in `content/published/js/` and reference them in `site-config.el`:

```elisp
(setq my-html-head
  "<link rel=\"stylesheet\" href=\"/static/css/custom.css\" />
<script src=\"/js/main.js\"></script>")
```

### Navigation

Modify `my-site-preamble` and `my-private-site-preamble` in `site-config.el`.

---

## Generator

This site uses [orgroam-publish](https://github.com/subsid/orgroam-publish), a static site generator for org-roam notes. The generator lives in `generator/` as a git submodule.

To update the generator:

```bash
cd generator
git pull origin main
cd ..
git add generator
git commit -m "Update orgroam-publish generator"
```

See the [generator README](generator/README.md) for more details on how it works.

---

## License

The build tooling and configuration in this repository are released under the [MIT License](LICENSE). This covers the software only — blog posts and notes remain personal content.

---

*Built with [orgroam-publish](https://github.com/subsid/orgroam-publish) • Inspired by [System Crafters org-website-example](https://github.com/SystemCrafters/org-website-example)*
