;; Site-specific configuration for subsid.github.io
;; This file overrides defaults from the generator

;; Point to your org-roam directory
(setq my-org-pages-dir
  (expand-file-name "~/Dropbox/notes/org_roam_v2/pages"))

;; Your custom navigation for public blog
(setq my-site-preamble
  "<div class=\"site-header\">
  <nav class=\"site-nav\">
    <a href=\"/about.html\">About</a>
    <a href=\"/notes.html\">Notes</a>
    <a href=\"/references.html\">References</a>
  </nav>
</div>")

;; Your custom navigation for private blog
(setq my-private-site-preamble
  "<div class=\"site-header\">
  <nav class=\"site-nav\">
    <a href=\"/about.html\">About</a>
    <a href=\"/notes.html\">Notes</a>
    <a href=\"/references.html\">References</a>
    <a href=\"/private.html\">Unpublished/Private</a>
  </nav>
</div>")
