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
    <a href=\"about.html\">About</a>
    <a href=\"articles.html\">Articles</a>
    <a href=\"snippets.html\">Snippets</a>
  </nav>
</div>")

(defvar my-private-site-preamble 
  "<div class=\"site-header\">
  <nav class=\"site-nav\">
    <a href=\"about.html\">About</a>
    <a href=\"articles.html\">Articles</a>
    <a href=\"snippets.html\">Snippets</a>
    <a href=\"private.html\">Unpublished/Private</a>
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
<link rel=\"stylesheet\" href=\"static/css/custom.css\" />
<script src=\"js/main.js\"></script>
<script>hljs.highlightAll();hljs.addPlugin(new CopyButtonPlugin());</script>
"
      org-html-preamble t
      org-html-preamble-format (list (list "en" my-site-preamble))
      )

;; Helper function to get file information
(defun get-file-tags-info (file-path)
  "Return a list of (has-publish has-sitemapignore has-snippet has-notitle) for FILE-PATH."
  (with-temp-buffer
    (insert-file-contents file-path)
    (let ((content (buffer-string)))
      (list (string-match-p "^#\\+TAGS:.*publish" content)
            (string-match-p "^#\\+TAGS:.*sitemapignore" content)
            (string-match-p "^#\\+TAGS:.*snippet" content)
            (string-match-p "^#\\+TAGS:.*notitle" content)))))

;; Unified publishing function
(defun my-html-publish-with-tags (plist filename pub-dir &optional filter-fn)
  "Publish to HTML based on optional FILTER-FN predicate.
   If FILTER-FN is nil, publish all files.
   Remove title from export if 'notitle' tag is present."
  (let* ((explicit-files '("articles.org" "snippets.org"))
         (file-info (get-file-tags-info filename))
         (has-notitle (nth 3 file-info))
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
  (let* ((date (org-publish-find-date entry project))
         (title (org-publish-find-title entry project))
         (date-str (if date
                      (format-time-string "%Y-%m-%d" date)
                    "No Date"))
         ;; Get the actual published filename
         (published-filename 
          (with-temp-buffer
            (insert-file-contents (expand-file-name entry 
                                                   (plist-get (cdr project) :base-directory)))
            (goto-char (point-min))
            (if (re-search-forward "^#\\+EXPORT_FILE_NAME:[ \t]*\\(.*\\)" nil t)
                (concat (match-string 1) ".html")
              (concat (file-name-sans-extension entry) ".html")))))
    (format "[[file:%s][%s]] /%s/"
            published-filename
            title
            date-str)))

(defun my-snippets-no-publish (plist filename pub-dir)
  "Don't publish individual snippet files - only generate sitemap."
  nil)

;; Unified sitemap function
(defun create-sitemap-with-description (title description list)
  "Create a sitemap with TITLE and DESCRIPTION, filtering nil entries from LIST."
  (concat "#+TITLE: " title "\n\n"
          description "\n\n"
          (org-list-to-org
           (cons (car list)  ; Keep list type
                 (seq-filter (lambda (item)
                               ;; Filter out nil items and lists containing only nil
                               (and item 
                                    (not (and (listp item) 
                                              (= (length item) 1) 
                                              (null (car item))))))
                             (cdr list))))))

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
  ("my-org-content"
   :recursive t
   :base-directory "~/Dropbox/notes/org_roam_v2/pages/article"
   :base-extension "org"
   :publishing-directory "./public"
   :publishing-function my-conditional-html-publish
         ,@common-html-properties
         :sitemap-filename "articles.org"
         :sitemap-title "Articles"
   :sitemap-format-entry (lambda (entry style project)
                        (let ((file-path (expand-file-name entry 
                                                          (plist-get (cdr project) :base-directory))))
                          (let ((file-info (get-file-tags-info file-path)))
                            (let ((has-publish (car file-info))
                                  (has-sitemapignore (cadr file-info))
                                  (has-snippet (caddr file-info)))
                            (when (and has-publish 
                                      (not has-sitemapignore)
                                      (not has-snippet))
                              (create-sitemap-entry entry project))))))
   :sitemap-function (lambda (title list)
                       (create-sitemap-with-description 
                        title 
                        "Welcome to my collection of articles."
                        list)))

  ("my-org-snippets"
   :recursive t
   :base-directory "~/Dropbox/notes/org_roam_v2/pages/article"
   :base-extension "org"
   :publishing-directory "./public"
   :publishing-function my-snippets-no-publish
         ,@common-html-properties
         :sitemap-filename "snippets.org"
         :sitemap-title "Snippets"
   :sitemap-format-entry (lambda (entry style project)
                        (let ((file-path (expand-file-name entry 
                                                          (plist-get (cdr project) :base-directory))))
                          (let ((file-info (get-file-tags-info file-path)))
                            (let ((has-publish (car file-info))
                                  (has-sitemapignore (cadr file-info))
                                  (has-snippet (caddr file-info)))
                            (when (and has-publish 
                                      (not has-sitemapignore)
                                      has-snippet)
                              (create-sitemap-entry entry project))))))
   :sitemap-function (lambda (title list)
                       (create-sitemap-with-description 
                        title 
                        "Quick tips, code snippets, and bite-sized insights."
                        list)))

  ;; Private blog project that exports ALL files
  ("my-private-blog-content"
   :recursive t
   :base-directory "~/Dropbox/notes/org_roam_v2/pages/article"
   :base-extension "org"
   :publishing-directory "./private"
   :publishing-function my-publish-all-html
   :html-preamble t
   :html-preamble-format (("en" ,my-private-site-preamble))
   ,@common-html-properties
   :sitemap-filename "private.org"
   :sitemap-title "Private Articles"
   :sitemap-format-entry (lambda (entry style project)
                      (let ((filename (file-name-nondirectory entry)))
                        ;; Exclude sitemap files themselves
                        (unless (member filename '("snippets.org" "articles.org" "archive.org" "private.org"))
                          (let* ((file-path (expand-file-name entry 
                                                            (plist-get (cdr project) :base-directory)))
                                 (file-info (get-file-tags-info file-path))
                                 (has-publish (car file-info))
                                 (has-sitemapignore (cadr file-info))
                                 (has-snippet (caddr file-info)))
                            ;; Include files that are NOT in public articles or snippets sitemaps
                            (when (and 
                                   ;; Don't include files that appear in public articles sitemap
                                   (not (and has-publish (not has-sitemapignore) (not has-snippet)))
                                   ;; Don't include files that appear in public snippets sitemap  
                                   (not (and has-publish (not has-sitemapignore) has-snippet)))
                              (create-sitemap-entry entry project))))))
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

  ("my-org-blog" :components ("my-org-static" "my-org-content" "my-org-snippets"))
  ("my-private-blog" :components ("my-private-unpublished-static" "my-private-blog-static" "my-private-blog-content"))
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
    <link rel=\"stylesheet\" href=\"static/css/custom.css\" />
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

;; Generate the site output
(org-publish-project "my-org-blog" t)
(create-index-redirect "./public" "articles.html" my-site-preamble "articles")

;; Generate the private blog
(org-publish-project "my-private-blog" t)
(create-index-redirect "./private" "private.html" my-private-site-preamble "all articles")

(message "Build complete - both public and private blogs generated")
