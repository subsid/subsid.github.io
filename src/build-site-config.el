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
  (let* ((explicit-files '("notes.org" "references.org"))
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

(defun has-tag-p (file-path tag)
  "Check if file has a specific tag."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TAGS:.*" nil t)
      (string-match-p (regexp-quote tag) (match-string 0)))))

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
             ;; Get the actual published filename
             (published-filename
              (with-temp-buffer
                (insert-file-contents full-path)
                (goto-char (point-min))
                (if (re-search-forward "^#\\+EXPORT_FILE_NAME:[ \t]*\\(.*\\)" nil t)
                    (concat (match-string 1) ".html")
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
(defun create-sitemap-with-description (title description list)
  "Create a sitemap with TITLE and DESCRIPTION, filtering nil entries from LIST."
  (let* ((entries (flatten-sitemap-list list))
         (filtered-entries (delq nil entries)))
    (concat "#+TITLE: " title "\n\n"
            description "\n\n"
            (if filtered-entries
                (org-list-to-org
                 (cons 'unordered
                       (mapcar (lambda (entry) (list entry)) filtered-entries)))
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

;; Define the publishing project
(setq org-publish-project-alist
      `(
  ;; Notes content from article/ and main/ directories
  ("my-org-notes-articles"
   :recursive t
   :base-directory "~/Dropbox/notes/org_roam_v2/pages/article"
   :base-extension "org"
   :publishing-directory "./public"
   :publishing-function my-conditional-html-publish
   ,@common-html-properties
   :auto-sitemap nil)

  ("my-org-notes-main"
   :recursive t
   :base-directory "~/Dropbox/notes/org_roam_v2/pages/main"
   :base-extension "org"
   :publishing-directory "./public"
   :publishing-function my-conditional-html-publish
   ,@common-html-properties
   :auto-sitemap nil)

  ;; Combined sitemap for notes (articles + main)
  ("my-org-notes-sitemap"
   :recursive t
   :base-directory "~/Dropbox/notes/org_roam_v2/pages"
   :base-extension "org"
   :publishing-directory "./public"
   :publishing-function my-conditional-html-publish
   ;; Exclude reference, project, work directories and Inbox.org
   :exclude "^\\(reference\\|project\\|work\\)/.*\\.org$\\|^Inbox\\.org$"
   ,@common-html-properties
   :sitemap-filename "notes.org"
   :sitemap-title "Notes"
   :sitemap-format-entry (lambda (entry style project)
                        (let ((file-path (expand-file-name entry
                                                          (plist-get (cdr project) :base-directory))))
                          ;; Only process regular files, skip directories
                          (when (file-regular-p file-path)
                            (let ((file-info (get-file-tags-info file-path)))
                              (when file-info  ; file-info will be nil for directories
                                (let ((has-publish (car file-info))
                                      (has-sitemapignore (cadr file-info)))
                                  (when (and has-publish
                                            (not has-sitemapignore))
                                    (create-sitemap-entry entry project))))))))
   :sitemap-function (lambda (title list)
                       (create-sitemap-with-description
                        title
                        "A collection of notes, articles, and snippets."
                        list)))

  ;; References content
  ("my-org-references-content"
   :recursive t
   :base-directory "~/Dropbox/notes/org_roam_v2/pages/reference"
   :base-extension "org"
   :publishing-directory "./public"
   :publishing-function my-conditional-html-publish
   ,@common-html-properties
   :sitemap-filename "references.org"
   :sitemap-title "References"
   :sitemap-format-entry (lambda (entry style project)
                        (let ((file-path (expand-file-name entry
                                                          (plist-get (cdr project) :base-directory))))
                          (when (file-regular-p file-path)
                            (let ((file-info (get-file-tags-info file-path)))
                              (when file-info
                                (let ((has-publish (car file-info))
                                      (has-sitemapignore (cadr file-info)))
                                  (when (and has-publish
                                            (not has-sitemapignore))
                                    (create-sitemap-entry entry project))))))))
   :sitemap-function (lambda (title list)
                       (create-sitemap-with-description
                        title
                        "Reference links/snippets/resources."
                        list)))

  ;; Private versions of notes and references (same content, published to ./private)
  ("my-private-notes-articles"
   :recursive t
   :base-directory "~/Dropbox/notes/org_roam_v2/pages/article"
   :base-extension "org"
   :publishing-directory "./private"
   :publishing-function my-conditional-html-publish
   :html-preamble t
   :html-preamble-format (("en" ,my-private-site-preamble))
   ,@common-html-properties
   :auto-sitemap nil)

  ("my-private-notes-main"
   :recursive t
   :base-directory "~/Dropbox/notes/org_roam_v2/pages/main"
   :base-extension "org"
   :publishing-directory "./private"
   :publishing-function my-conditional-html-publish
   :html-preamble t
   :html-preamble-format (("en" ,my-private-site-preamble))
   ,@common-html-properties
   :auto-sitemap nil)

  ("my-private-notes-sitemap"
   :recursive t
   :base-directory "~/Dropbox/notes/org_roam_v2/pages"
   :base-extension "org"
   :publishing-directory "./private"
   :publishing-function my-conditional-html-publish
   :exclude "^\\(reference\\|project\\|work\\)/.*\\.org$\\|^Inbox\\.org$"
   :html-preamble t
   :html-preamble-format (("en" ,my-private-site-preamble))
   ,@common-html-properties
   :sitemap-filename "notes.org"
   :sitemap-title "Notes"
   :sitemap-format-entry (lambda (entry style project)
                        (let ((file-path (expand-file-name entry
                                                          (plist-get (cdr project) :base-directory))))
                          (when (file-regular-p file-path)
                            (let ((file-info (get-file-tags-info file-path)))
                              (when file-info
                                (let ((has-publish (car file-info))
                                      (has-sitemapignore (cadr file-info)))
                                  (when (and has-publish
                                            (not has-sitemapignore))
                                    (create-sitemap-entry entry project))))))))
   :sitemap-function (lambda (title list)
                       (create-sitemap-with-description
                        title
                        "A collection of notes, articles, and snippets."
                        list)))

  ("my-private-references-content"
   :recursive t
   :base-directory "~/Dropbox/notes/org_roam_v2/pages/reference"
   :base-extension "org"
   :publishing-directory "./private"
   :publishing-function my-conditional-html-publish
   :html-preamble t
   :html-preamble-format (("en" ,my-private-site-preamble))
   ,@common-html-properties
   :sitemap-filename "references.org"
   :sitemap-title "References"
   :sitemap-format-entry (lambda (entry style project)
                        (let ((file-path (expand-file-name entry
                                                          (plist-get (cdr project) :base-directory))))
                          (when (file-regular-p file-path)
                            (let ((file-info (get-file-tags-info file-path)))
                              (when file-info
                                (let ((has-publish (car file-info))
                                      (has-sitemapignore (cadr file-info)))
                                  (when (and has-publish
                                            (not has-sitemapignore))
                                    (create-sitemap-entry entry project))))))))
   :sitemap-function (lambda (title list)
                       (create-sitemap-with-description
                        title
                        "Reference materials and resources."
                        list)))

  ;; Private blog project that exports ALL files (unpublished content)
  ("my-private-blog-content"
   :recursive t
   :base-directory "~/Dropbox/notes/org_roam_v2/pages"
   :base-extension "org"
   :publishing-directory "./private"
   :publishing-function my-publish-all-html
   :html-preamble t
   :html-preamble-format (("en" ,my-private-site-preamble))
   ;; Exclude project, work directories and Inbox.org (but include reference for private)
   :exclude "^\\(project\\|work\\)/.*\\.org$\\|^Inbox\\.org$"
   ,@common-html-properties
   :sitemap-filename "private.org"
   :sitemap-title "Private Articles"
   :sitemap-format-entry (lambda (entry style project)
                      (let ((filename (file-name-nondirectory entry)))
                        ;; Exclude sitemap files themselves
                        (unless (member filename '("notes.org" "references.org" "archive.org" "private.org"))
                          (let* ((file-path (expand-file-name entry
                                                            (plist-get (cdr project) :base-directory))))
                            (when (file-regular-p file-path)
                              (let ((file-info (get-file-tags-info file-path)))
                                (when file-info
                                  (let ((has-publish (car file-info))
                                        (has-sitemapignore (cadr file-info)))
                                    ;; Include files that are NOT in public notes sitemap
                                    (when (not (and has-publish (not has-sitemapignore)))
                                      (create-sitemap-entry entry project))))))))))
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
   :publishing-function org-publish-attachment
   )

  ("my-private-blog-static"
   :base-directory "./content/published"
   :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|html"
   :publishing-directory "./private"
   :recursive t
   :publishing-function org-publish-attachment
   )

  ("my-private-unpublished-static"
   :base-directory "./content/unpublished"
   :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|html"
   :publishing-directory "./private"
   :recursive t
   :publishing-function org-publish-attachment
  )

  ("my-org-blog" :components ("my-org-static" "my-org-notes-articles" "my-org-notes-main" "my-org-notes-sitemap" "my-org-references-content"))
  ("my-private-blog" :components ("my-private-unpublished-static" "my-private-blog-static" "my-private-notes-articles" "my-private-notes-main" "my-private-notes-sitemap" "my-private-references-content" "my-private-blog-content"))
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
