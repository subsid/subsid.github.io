;; package installation setup for the website

; I have 2 versions of the website
; 1. Notes that need to be published to the public directory
; 2. Notes that need to be published to the private directory

(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
    ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(package-install 'htmlize)

;; Load the publishing system
(require 'ox-publish)

;; Source directory for org files.
;; Override by setting the ORG_PAGES_DIR environment variable.
;; Defaults to test/fixtures/pages so a fresh clone works out of the box.
(defvar my-org-pages-dir
  (expand-file-name
   (or (getenv "ORG_PAGES_DIR") "./test/fixtures/pages"))
  "Root directory containing org source files (article/, main/, reference/).")

;; Define preambles
(defvar my-site-preamble
  "<div class=\"site-header\">
  <nav class=\"site-nav\">
    <a href=\"/about.html\">About</a>
    <a href=\"/notes.html\">Notes</a>
    <a href=\"/references.html\">References</a>
  </nav>
</div>")

(defvar my-private-site-preamble
  "<div class=\"site-header\">
  <nav class=\"site-nav\">
    <a href=\"/about.html\">About</a>
    <a href=\"/notes.html\">Notes</a>
    <a href=\"/references.html\">References</a>
    <a href=\"/private.html\">Unpublished/Private</a>
  </nav>
</div>")

;; Javascript/CSS I include
;; Minimal styling with simple.css
;; Code highlighting and copy badge - highlight.js and highlighjs-copy
(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />
<link rel=\"stylesheet\" href=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/styles/a11y-dark.min.css\" integrity=\"sha512-Vj6gPCk8EZlqnoveEyuGyYaWZ1+jyjMPg8g4shwyyNlRQl6d3L9At02ZHQr5K6s5duZl/+YKMnM3/8pDhoUphg==\" crossorigin=\"anonymous\" referrerpolicy=\"no-referrer\" />
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/highlight.min.js\"></script>
<!-- and it's easy to individually load additional languages -->
<script src=\"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/languages/go.min.js\"></script>
<script src=\"https://unpkg.com/highlightjs-copy/dist/highlightjs-copy.min.js\"></script>
<link
  rel=\"stylesheet\"
  href=\"https://unpkg.com/highlightjs-copy/dist/highlightjs-copy.min.css\"
/>
<link rel=\"stylesheet\" href=\"/static/css/custom.css\" />
<script src=\"/js/main.js\"></script>
<script>hljs.highlightAll();hljs.addPlugin(new CopyButtonPlugin());</script>
"
      org-html-preamble t
      org-html-preamble-format (list (list "en" my-site-preamble))
      )

;; Helper function to get file information
(defun get-file-tags-info (file-path)
  "Return a list of (has-publish has-sitemapignore has-notitle) for FILE-PATH."
  (when (file-regular-p file-path)  ; Only process regular files, not directories
    (with-temp-buffer
      (insert-file-contents file-path)
      (let ((content (buffer-string)))
        (list (string-match-p "^#\\+TAGS:.*publish" content)
              (string-match-p "^#\\+TAGS:.*sitemapignore" content)
              (string-match-p "^#\\+TAGS:.*notitle" content))))))

;; Unified publishing function
(defun my-html-publish-with-tags (plist filename pub-dir &optional filter-fn)
  "Publish to HTML based on optional FILTER-FN predicate.
   If FILTER-FN is nil, publish all files.
   Remove title from export if 'notitle' tag is present."
  (let* ((explicit-files my-public-sitemap-files)
         (file-info (get-file-tags-info filename))
         (has-notitle (nth 2 file-info))
         (should-publish (if filter-fn
                             (funcall filter-fn filename file-info explicit-files)
                           t)))
    (when should-publish
      (message "Exporting to blog %s" filename)
      (let ((message-log-max nil)
            (inhibit-message t)
            (export-plist (if has-notitle
                              (plist-put (copy-sequence plist) :with-title nil)
                            plist)))
        (org-html-publish-to-html export-plist filename pub-dir)))))

;; Specific filter for conditional publishing
(defun my-conditional-html-publish (plist filename pub-dir)
  "Publish to HTML if file has 'publish' tag or is in explicit filename list."
  (my-html-publish-with-tags
   plist filename pub-dir
   (lambda (filename file-info explicit-files)
     (or (car file-info)  ; has-publish
         (member (file-name-nondirectory filename) explicit-files)))))

;; Publish all files without filtering
(defun my-publish-all-html (plist filename pub-dir)
  "Publish ALL org files to HTML without any tag filtering."
  (my-html-publish-with-tags plist filename pub-dir nil))

;; Unified sitemap entry creation
(defun create-sitemap-entry (entry project)
  "Create a sitemap entry for a file."
  (let ((full-path (expand-file-name entry (plist-get (cdr project) :base-directory))))
    ;; Only process if it's a regular file, not a directory
    (when (file-regular-p full-path)
      (let* ((date (org-publish-find-date entry project))
             (title (org-publish-find-title entry project))
             (date-str (if date
                          (format-time-string "%Y-%m-%d" date)
                        "No Date"))
             ;; Get the actual published filename, preserving subdirectory from entry
             (published-filename
               (with-temp-buffer
                 (insert-file-contents full-path)
                 (goto-char (point-min))
                 (if (re-search-forward "^#\\+EXPORT_FILE_NAME:[ \t]*\\(.*\\)" nil t)
                     (concat (file-name-directory entry) (match-string 1) ".html")
                   (concat (file-name-sans-extension entry) ".html")))))
        (format "[[file:%s][%s]] /%s/"
                published-filename
                title
                date-str)))))

;; Helper function to flatten nested list and extract only string entries
(defun flatten-sitemap-list (lst)
  "Flatten nested list LST and extract only string entries (the actual links)."
  (cond
   ((null lst) nil)
   ((stringp lst) (list lst))
   ((not (listp lst)) nil)
   (t (apply #'append (mapcar #'flatten-sitemap-list lst)))))

;; Unified sitemap function
(defun create-sitemap-with-description (title description list &optional pinned)
  "Create a sitemap with TITLE and DESCRIPTION, filtering nil entries from LIST.
If PINNED is a string, any entry whose link contains PINNED is moved to the front."
  (let* ((entries (flatten-sitemap-list list))
         (filtered-entries (delq nil entries))
         (ordered-entries
          (if pinned
              (let ((pinned-entries (seq-filter (lambda (e) (string-match-p (regexp-quote pinned) e)) filtered-entries))
                    (rest-entries   (seq-remove  (lambda (e) (string-match-p (regexp-quote pinned) e)) filtered-entries)))
                (append pinned-entries rest-entries))
            filtered-entries)))
    (concat "#+TITLE: " title "\n\n"
            description "\n\n"
            (if ordered-entries
                (org-list-to-org
                 (cons 'unordered
                       (mapcar (lambda (entry) (list entry)) ordered-entries)))
              ""))))

;; Common project properties
(defvar common-html-properties
  '(:html-metadata-timestamp-format "%B %d, %Y"
    :html-postamble t
    :html-postamble-format (("en" "<p class=\"date\"><i>Last Modified: %C</i></p>"))
    :with-author nil
    :with-creator t
    :with-toc t
    :section-numbers nil
    :time-stamp-file nil
    :auto-sitemap t
    :sitemap-sort-files anti-chronologically))

;; Sitemap org files generated by ox-publish — always published regardless of tags
(defvar my-public-sitemap-files '("notes.org" "references.org")
  "Generated sitemap files for the public blog.")
(defvar my-private-sitemap-files '("notes.org" "references.org" "private.org")
  "Generated sitemap files for the private blog.")

;; Directories to always exclude from publishing
;; project/ and work/ are GTD task dirs; Inbox.org is a scratch file
(defvar my-exclude-always "^\\(project\\|work\\)/.*\\.org$\\|^Inbox\\.org$\\|^Mobile Inbox\\.org$")
(defvar my-exclude-reference "^reference/.*\\.org$")
(defvar my-exclude-notes "^\\(article\\|main\\)/.*\\.org$")

;; Sitemap format entry for notes/articles (only publish-tagged, non-sitemapignore files)
(defun my-notes-sitemap-format-entry (entry style project)
  "Format a sitemap entry for notes, including only published non-ignored files."
  (let ((file-path (expand-file-name entry (plist-get (cdr project) :base-directory))))
    (when (file-regular-p file-path)
      (let ((file-info (get-file-tags-info file-path)))
        (when file-info
          (let ((has-publish (car file-info))
                (has-sitemapignore (cadr file-info)))
            (when (and has-publish (not has-sitemapignore))
              (create-sitemap-entry entry project))))))))

;; Sitemap format entry for private unpublished content
(defun my-private-sitemap-format-entry (entry style project)
  "Format a sitemap entry for private content not on the public blog."
  (let ((filename (file-name-nondirectory entry)))
    ;; Skip the generated sitemap org files themselves
    (unless (member filename my-private-sitemap-files)
      (let ((file-path (expand-file-name entry (plist-get (cdr project) :base-directory))))
        (when (file-regular-p file-path)
          (let ((file-info (get-file-tags-info file-path)))
            (when file-info
              (let ((has-publish (car file-info))
                    (has-sitemapignore (cadr file-info)))
                ;; Include files NOT already on the public sitemap
                (when (not (and has-publish (not has-sitemapignore)))
                  (create-sitemap-entry entry project))))))))))

;; Define the publishing projects
(setq org-publish-project-alist
      `(
  ;; Public notes: article/ and main/ -> public/article/, public/main/
  ("my-org-notes"
   :recursive t
   :base-directory ,my-org-pages-dir
   :base-extension "org"
   :publishing-directory "./public"
   :publishing-function my-conditional-html-publish
   :exclude ,(concat my-exclude-always "\\|" my-exclude-reference)
   ,@common-html-properties
   :sitemap-filename "notes.org"
   :sitemap-title "Notes"
   :sitemap-format-entry my-notes-sitemap-format-entry
   :sitemap-function (lambda (title list)
                       (create-sitemap-with-description
                        title
                        "A collection of notes, articles, and snippets."
                        list)))

  ;; Public references: reference/ -> public/reference/
  ("my-org-references"
   :recursive t
   :base-directory ,my-org-pages-dir
   :base-extension "org"
   :publishing-directory "./public"
   :publishing-function my-conditional-html-publish
   :exclude ,(concat my-exclude-always "\\|" my-exclude-notes)
   ,@common-html-properties
   :sitemap-filename "references.org"
   :sitemap-title "References"
   :sitemap-format-entry my-notes-sitemap-format-entry
   :sitemap-function (lambda (title list)
                       (create-sitemap-with-description
                        title
                        "Reference links/snippets/resources."
                        list
                        "useful-links")))

  ;; Private notes: article/ and main/ -> private/article/, private/main/
  ("my-private-notes"
   :recursive t
   :base-directory ,my-org-pages-dir
   :base-extension "org"
   :publishing-directory "./private"
   :publishing-function my-conditional-html-publish
   :exclude ,(concat my-exclude-always "\\|" my-exclude-reference)
   :html-preamble t
   :html-preamble-format (("en" ,my-private-site-preamble))
   ,@common-html-properties
   :sitemap-filename "notes.org"
   :sitemap-title "Notes"
   :sitemap-format-entry my-notes-sitemap-format-entry
   :sitemap-function (lambda (title list)
                       (create-sitemap-with-description
                        title
                        "A collection of notes, articles, and snippets."
                        list)))

  ;; Private references: reference/ -> private/reference/
  ("my-private-references"
   :recursive t
   :base-directory ,my-org-pages-dir
   :base-extension "org"
   :publishing-directory "./private"
   :publishing-function my-conditional-html-publish
   :exclude ,(concat my-exclude-always "\\|" my-exclude-notes)
   :html-preamble t
   :html-preamble-format (("en" ,my-private-site-preamble))
   ,@common-html-properties
   :sitemap-filename "references.org"
   :sitemap-title "References"
   :sitemap-format-entry my-notes-sitemap-format-entry
   :sitemap-function (lambda (title list)
                       (create-sitemap-with-description
                        title
                        "Reference materials and resources."
                        list
                        "useful-links")))

  ;; Private unpublished: all files not on public blog -> private/
  ("my-private-blog-content"
   :recursive t
   :base-directory ,my-org-pages-dir
   :base-extension "org"
   :publishing-directory "./private"
   :publishing-function my-publish-all-html
   :exclude ,my-exclude-always
   :html-preamble t
   :html-preamble-format (("en" ,my-private-site-preamble))
   ,@common-html-properties
   :sitemap-filename "private.org"
   :sitemap-title "Private Articles"
   :sitemap-format-entry my-private-sitemap-format-entry
   :sitemap-function (lambda (title list)
                       (create-sitemap-with-description
                        title
                        "Articles not published on the public blog."
                        list)))

  ("my-org-static"
   :base-directory "./content/published"
   :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|html"
   :publishing-directory "./public"
   :recursive t
   :publishing-function org-publish-attachment)

  ("my-private-blog-static"
   :base-directory "./content/published"
   :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|html"
   :publishing-directory "./private"
   :recursive t
   :publishing-function org-publish-attachment)

  ("my-private-unpublished-static"
   :base-directory "./content/unpublished"
   :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|html"
   :publishing-directory "./private"
   :recursive t
   :publishing-function org-publish-attachment)

  ("my-org-blog" :components ("my-org-static" "my-org-notes" "my-org-references"))
  ("my-private-blog" :components ("my-private-unpublished-static" "my-private-blog-static" "my-private-notes" "my-private-references" "my-private-blog-content"))
  )
      )

;; Unified index redirect creation
(defun create-index-redirect (directory target-page preamble description)
  "Create an index.html redirect in DIRECTORY to TARGET-PAGE with PREAMBLE and DESCRIPTION."
  (let ((index-content (format "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta http-equiv=\"refresh\" content=\"0; url=%s\">
    <title>Redirecting...</title>
    <link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />
    <link rel=\"stylesheet\" href=\"/static/css/custom.css\" />
</head>
<body>
    %s
    <main>
        <p>Redirecting to <a href=\"%s\">%s</a>...</p>
    </main>
</body>
</html>" target-page preamble target-page description)))
    (with-temp-file (concat directory "/index.html")
      (insert index-content))))

;; I did not write this, I found it from a stackoverflow post but I am unable to find a link to it
(defun my/org-html-src-block (html)
  "Modify the output of org-html-src-block for highlight.js"
  (replace-regexp-in-string
   "</pre>" "</code></pre>"
   (replace-regexp-in-string
    "<pre class=\"src src-\\(.*\\)\">"
    "<pre><code class=\"\\1\">"
    html)))

(advice-add 'org-html-src-block :filter-return #'my/org-html-src-block)

;; Clear the org-publish cache to avoid cache errors
(setq org-publish-cache nil)
(when (file-exists-p org-publish-timestamp-directory)
  (delete-directory org-publish-timestamp-directory t))
