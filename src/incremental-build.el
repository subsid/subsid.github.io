;; Incremental build script - only publishes a single changed file
;; Usage: emacs -Q --batch --eval "(setq changed-file \"/path/to/file.org\")" -l incremental-build.el

(require 'package)
(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
    ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'htmlize)
  (package-install 'htmlize))

(require 'ox-publish)

;; Load the configuration (but not the build commands)
(load-file "./src/build-site-config.el")

;; Publish only the changed file
(when (boundp 'changed-file)
  (message "Publishing single file: %s" changed-file)
  
  ;; Initialize cache for each project - required for org-publish-file to work
  (dolist (project '("my-org-notes-articles" "my-org-notes-main" "my-private-notes-articles" "my-private-notes-main" "my-org-references-content" "my-private-references-content"))
    (org-publish-initialize-cache project))
  
  ;; Publish the file - it will be published to all projects it belongs to
  (org-publish-file changed-file)
  
  ;; Write caches back
  (org-publish-write-cache-file))

(message "Incremental build complete!")
