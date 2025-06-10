;; package installation setup for the website

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

;; Define the preamble as a variable
(defvar my-site-preamble 
  "<div class=\"site-header\">
  <nav class=\"site-nav\">
    <a href=\"about.html\">About</a>
    <a href=\"articles.html\">Articles</a>
    <a href=\"snippets.html\">Snippets</a>
  </nav>
</div>")

;; Define the preamble as a variable
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

(defun my-conditional-html-publish (plist filename pub-dir)
  "Publish to HTML if file has 'publish' tag or is in explicit filename list.
   Remove title from export if 'notitle' tag is present."
  (let ((explicit-files '("articles.org" "snippets.org")))
    (with-temp-buffer
      (insert-file-contents filename)
      (goto-char (point-min))
      (when (or (re-search-forward "^#\\+TAGS:.*publish" nil t)
                (member (file-name-nondirectory filename) explicit-files))
 	(message "Exporting to blog %s" filename)
        ;; Check for notitle tag
        (goto-char (point-min))
        (let ((has-notitle-tag (re-search-forward "^#\\+TAGS:.*notitle" nil t))
              (message-log-max nil)
              (inhibit-message t))
          ;; Modify plist to exclude title if notitle tag is present
          (let ((export-plist (if has-notitle-tag
                                  (plist-put (copy-sequence plist) :with-title nil)
                                plist)))
            (org-html-publish-to-html export-plist filename pub-dir)))))))

;; NEW: Function to publish ALL files without filtering
(defun my-publish-all-html (plist filename pub-dir)
  "Publish ALL org files to HTML without any tag filtering.
   Remove title from export if 'notitle' tag is present."
  (message "Exporting to private blog %s" filename)
  (with-temp-buffer
    (insert-file-contents filename)
    (goto-char (point-min))
    ;; Check for notitle tag
    (let ((has-notitle-tag (re-search-forward "^#\\+TAGS:.*notitle" nil t))
          (message-log-max nil)
          (inhibit-message t))
      ;; Modify plist to exclude title if notitle tag is present
      (let ((export-plist (if has-notitle-tag
                              (plist-put (copy-sequence plist) :with-title nil)
                            plist)))
        (org-html-publish-to-html export-plist filename pub-dir)))))

(defun has-tag-p (file-path tag)
  "Check if file has a specific tag."
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))
    (when (re-search-forward "^#\\+TAGS:.*" nil t)
      (string-match-p (regexp-quote tag) (match-string 0)))))

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

;; NEW: Function to create sitemap entry for ALL files
(defun create-sitemap-entry-all (entry project)
  "Create a sitemap entry for ANY file (no filtering)."
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

;; Define the publishing project
(setq org-publish-project-alist
      '(
	("my-org-content"
	 :recursive t
	 :base-directory "~/Dropbox/notes/org_roam_v2/pages/article"
	 :base-extension "org"
	 :publishing-directory "./public"
	 :publishing-function my-conditional-html-publish
         :html-metadata-timestamp-format "%B %d, %Y"
	 :html-postamble t
	 :html-postamble-format (("en" "<p class=\"date\"><i>Last Modified: %C</i></p>"))
	 :with-author nil
	 :with-creator t
	 :with-toc t
	 :section-numbers nil
	 :time-stamp-file nil
	 :auto-sitemap t
         :sitemap-filename "articles.org"
         :sitemap-title "Articles"
         :sitemap-sort-files anti-chronologically
	 :sitemap-format-entry (lambda (entry style project)
                        ;; Check if file has publish tag and doesn't have sitemapignore tag
                        ;; AND doesn't have snippet tag (articles only)
                        (let ((file-path (expand-file-name entry 
                                                          (plist-get (cdr project) :base-directory))))
                          (let ((file-info
                                 (with-temp-buffer
                                   (insert-file-contents file-path)
                                   (let ((content (buffer-string)))
                                     (list (string-match-p "^#\\+TAGS:.*publish" content)
                                           (string-match-p "^#\\+TAGS:.*sitemapignore" content)
                                           (string-match-p "^#\\+TAGS:.*snippet" content))))))
                            ;; Only process if file has publish tag AND doesn't have sitemapignore tag AND doesn't have snippet tag
                            (when (and (car file-info) 
                                      (not (cadr file-info))
                                      (not (caddr file-info)))
                              (create-sitemap-entry entry project)))))
	 :sitemap-function (lambda (title list)
			     (concat "#+TITLE: " title "\n\n"
				     "Long-form thoughts.\n\n"
				     (org-list-to-org
				      (cons (car list)  ; Keep list type
					    (seq-filter (lambda (item)
							  ;; Filter out nil items and lists containing only nil
							  (and item 
							       (not (and (listp item) 
									 (= (length item) 1) 
									 (null (car item))))))
							(cdr list))))))
	 )
	
	;; Second sitemap for snippets
	("my-org-snippets"
	 :recursive t
	 :base-directory "~/Dropbox/notes/org_roam_v2/pages/article"
	 :base-extension "org"
	 :publishing-directory "./public"
	 :publishing-function my-snippets-no-publish
         :auto-sitemap t
         :sitemap-filename "snippets.org"
         :sitemap-title "Snippets"
         :sitemap-sort-files anti-chronologically
	 :sitemap-format-entry (lambda (entry style project)
                        ;; Check if file has publish tag, doesn't have sitemapignore tag
                        ;; AND has snippet tag (snippets only)
                        (let ((file-path (expand-file-name entry 
                                                          (plist-get (cdr project) :base-directory))))
                          (let ((file-info
                                 (with-temp-buffer
                                   (insert-file-contents file-path)
                                   (let ((content (buffer-string)))
                                     (list (string-match-p "^#\\+TAGS:.*publish" content)
                                           (string-match-p "^#\\+TAGS:.*sitemapignore" content)
                                           (string-match-p "^#\\+TAGS:.*snippet" content))))))
                            ;; Only process if file has publish tag AND doesn't have sitemapignore tag AND has snippet tag
                            (when (and (car file-info) 
                                      (not (cadr file-info))
                                      (caddr file-info))
                              (create-sitemap-entry entry project)))))
	 :sitemap-function (lambda (title list)
			     (concat "#+TITLE: " title "\n\n"
				     "Quick tips, code snippets, and bite-sized insights.\n\n"
				     (org-list-to-org
				      (cons (car list)  ; Keep list type
					    (seq-filter (lambda (item)
							  ;; Filter out nil items and lists containing only nil
							  (and item 
							       (not (and (listp item) 
									 (= (length item) 1) 
									 (null (car item))))))
							(cdr list))))))
	 )

  ;; NEW: Private blog project that exports ALL files but sitemap excludes published ones
  ("my-private-blog-content"
   :recursive t
   :base-directory "~/Dropbox/notes/org_roam_v2/pages/article"
   :base-extension "org"
   :publishing-directory "./private"
   :publishing-function my-publish-all-html
   :html-metadata-timestamp-format "%B %d, %Y"
   :html-preamble t
   :html-preamble-format (("en" "<div class=\"site-header\">
    <nav class=\"site-nav\">
      <a href=\"about.html\">About</a>
      <a href=\"articles.html\">Articles</a>
      <a href=\"snippets.html\">Snippets</a>
      <a href=\"private.html\">Private Articles</a>
    </nav>
  </div>"))
   :html-postamble t
   :html-postamble-format (("en" "<p class=\"date\"><i>Last Modified: %C</i></p>"))
   :with-author nil
   :with-creator t
   :with-toc t
   :section-numbers nil
   :time-stamp-file nil
   :auto-sitemap t
   :sitemap-filename "private.org"
   :sitemap-title "Private Articles"
   :sitemap-sort-files anti-chronologically
   :sitemap-format-entry (lambda (entry style project)
                      ;; Only include files that are NOT published in public blog
                      (let ((filename (file-name-nondirectory entry)))
                        ;; Exclude sitemap files themselves
                        (unless (or (string= filename "snippets.org")
                                   (string= filename "articles.org")
                                   (string= filename "archive.org")
                                   (string= filename "private.org"))
                          (let ((file-path (expand-file-name entry 
                                                            (plist-get (cdr project) :base-directory))))
                            (let ((file-info
                                   (with-temp-buffer
                                     (insert-file-contents file-path)
                                     (let ((content (buffer-string)))
                                       (list (string-match-p "^#\\+TAGS:.*publish" content)
                                             (string-match-p "^#\\+TAGS:.*sitemapignore" content)
                                             (string-match-p "^#\\+TAGS:.*snippet" content))))))
                              ;; Include files that either:
                              ;; 1. Don't have publish tag (unpublished)
                              ;; 2. Have sitemapignore tag (excluded from public sitemaps)
                              ;; But exclude files that would appear in public articles or snippets
                              (let ((has-publish (car file-info))
                                    (has-sitemapignore (cadr file-info))
                                    (has-snippet (caddr file-info)))
                                (when (and 
                                       ;; Don't include files that appear in public articles sitemap
                                       (not (and has-publish (not has-sitemapignore) (not has-snippet)))
                                       ;; Don't include files that appear in public snippets sitemap  
                                       (not (and has-publish (not has-sitemapignore) has-snippet)))
                                  (create-sitemap-entry-all entry project))))))))
   :sitemap-function (lambda (title list)
           (concat "#+TITLE: " title "\n\n"
             "Articles not published on the public blog.\n\n"
             (org-list-to-org
              (cons (car list)  ; Keep list type
              (seq-filter (lambda (item)
                ;; Filter out nil items and lists containing only nil
                (and item 
                     (not (and (listp item) 
                   (= (length item) 1) 
                   (null (car item))))))
              (cdr list))))))
   )

	("my-org-static"
	 :base-directory "./content"
	 :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "./public"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )

	;; NEW: Static files for private blog
	("my-private-blog-static"
	 :base-directory "./content"
	 :base-extension "css\\|js\\|png\\|jpg\\|jpeg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
	 :publishing-directory "./private"
	 :recursive t
	 :publishing-function org-publish-attachment
	 )

	("my-org-blog" :components ("my-org-static" "my-org-content" "my-org-snippets"))
	("my-private-blog" :components ("my-private-blog-static" "my-private-blog-content"))
	)
      )


(defun create-index-redirect ()
  "Create an index.html that redirects to articles.html"
  (let ((index-content (format "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta http-equiv=\"refresh\" content=\"0; url=articles.html\">
    <title>Redirecting...</title>
    <link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />
    <link rel=\"stylesheet\" href=\"static/css/custom.css\" />
</head>
<body>
    %s
    <main>
        <p>Redirecting to <a href=\"articles.html\">articles</a>...</p>
    </main>
</body>
</html>" my-site-preamble)))
    (with-temp-file "./public/index.html"
      (insert index-content))))

;; NEW: Create index redirect for private blog
(defun create-private-index-redirect ()
  "Create an index.html for private blog that redirects to private.html"
  (let ((index-content (format "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta http-equiv=\"refresh\" content=\"0; url=private.html\">
    <title>Redirecting...</title>
    <link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" />
    <link rel=\"stylesheet\" href=\"static/css/custom.css\" />
</head>
<body>
    %s
    <main>
        <p>Redirecting to <a href=\"private.html\">all articles</a>...</p>
    </main>
</body>
</html>" my-private-site-preamble)))
    (with-temp-file "./private/index.html"
      (insert index-content))))

;; ;; I did not write this , i found it from a stackoverflow post but i am unable to find a link to it
 (defun my/org-html-src-block (html)
  "Modify the output of org-html-src-block for highlight.js"
  (replace-regexp-in-string
   "</pre>" "</code></pre>"
   (replace-regexp-in-string
    "<pre class=\"src src-\\(.*\\)\">"
    "<pre><code class=\"\\1\">"
    html)))

(advice-add 'org-html-src-block :filter-return #'my/org-html-src-block)

;; Generate the site output
(org-publish-project "my-org-blog" t)
(create-index-redirect)

;; NEW: Generate the private blog
(org-publish-project "my-private-blog" t)
(create-private-index-redirect)

(message "Build complete - both public and private blogs generated")
