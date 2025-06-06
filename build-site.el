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

(setq org-html-validation-link nil
      org-html-head-include-scripts nil
      org-html-head-include-default-style nil
      org-html-head "<link rel=\"stylesheet\" href=\"https://cdn.simplecss.org/simple.min.css\" /><link rel=\"stylesheet\" href=\"static/css/custom.css\" /><script src=\"js/main.js\"></script>"
      org-html-preamble t
      org-html-preamble-format '(("en" "<div class=\"site-header\">
  <nav class=\"site-nav\">
    <a href=\"about.html\">About</a>
    <a href=\"archive.html\">Archive</a>
  </nav>
</div>"))
     
      )

(defun my-conditional-html-publish (plist filename pub-dir)
  "Publish to HTML if file has 'publish' tag or is in explicit filename list.
   Remove title from export if 'notitle' tag is present."
  (let ((explicit-files '("archive.org")))
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
	 :html-postamble-format (("en" "<p class=\"date\">Last Modified: %C</p>"))
	 :with-author nil
	 :with-creator t
	 :with-toc t
	 :section-numbers nil
	 :time-stamp-file nil
	 :auto-sitemap t
         :sitemap-filename "archive.org"
         :sitemap-title "Archive"
         :sitemap-sort-files anti-chronologically
	 ;; (message "Entry: %s, Date: %s Date Str: %s, Published: %s" entry date date-str published-filename)
	 :sitemap-format-entry (lambda (entry style project)
                        ;; Check if file has publish tag and doesn't have sitemapignore tag
                        (let ((file-info
                               (with-temp-buffer
                                 (insert-file-contents (expand-file-name entry 
                                                                        (plist-get (cdr project) :base-directory)))
                                 (let ((content (buffer-string)))
                                   (list (string-match-p "^#\\+TAGS:.*publish" content)
                                         (string-match-p "^#\\+TAGS:.*sitemapignore" content))))))
                          ;; Only process if file has publish tag AND doesn't have sitemapignore tag
                          (when (and (car file-info) (not (cadr file-info)))
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
                                      date-str)))))
	 :sitemap-function (lambda (title list)
			     (concat "#+TITLE: " title "\n\n"
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

	("my-org-blog" :components ("my-org-static" "my-org-content"))
	)
)

;; Generate the site output
(org-publish-project "my-org-blog" t)

(message "Build complete")
