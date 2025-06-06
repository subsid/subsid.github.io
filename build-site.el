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


;; Define the publishing project
(setq org-publish-project-alist
      '(
	("my-org-content"
	 :recursive t
	 :base-directory "./content"
	 :base-extension "org"
	 :publishing-directory "./public"
	 :publishing-function org-html-publish-to-html
         :html-metadata-timestamp-format "%B %d, %Y"
	 :html-postamble t
	 :html-postamble-format (("en" "<p class=\"date\">Created: %d</p>"))
	 :with-author nil
	 :with-creator t
	 :with-toc t
	 :section-numbers nil
	 :time-stamp-file nil
	 :auto-sitemap t
         :sitemap-filename "archive.org"
         :sitemap-title "Archive"
         :sitemap-sort-files anti-chronologically
;	 :sitemap-format-entry (lambda (entry style project)
;				 (unless (string-match "about\\.org" entry)
;				   (format "[[file:%s][%s]] /%s/"
;					   entry
;					   (org-publish-find-title entry project)
;					   (format-time-string "%Y-%m-%d"
;							       (org-publish-find-date entry project)))))

	 :sitemap-format-entry (lambda (entry style project)
				 (unless (string-match "about\\.org" entry)
				   (let* ((date (org-publish-find-date entry project))
					  (title (org-publish-find-title entry project))
					  (date-str (if date
							(format-time-string "%Y-%m-%d" date)
						      "No Date"))
					  )
				     (message "Entry: %s, Date: %s Date Str: %s" entry date date-str)
				     (format "[[file:%s][%s]] /%s/"
					     entry
					     title
					     date-str))))
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
