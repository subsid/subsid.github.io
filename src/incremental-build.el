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

;; Publish a file into a specific named project by temporarily making it the
;; only project, so org-publish-file matches and publishes it correctly.
(defun my/publish-file-to-project (file project-name)
  "Publish FILE into PROJECT-NAME, initializing that project's cache first.
Computes the correct pub-dir by preserving the relative path from the
project's base directory, matching what org-publish-file does."
  (let ((project (assoc project-name org-publish-project-alist)))
    (when project
      (org-publish-initialize-cache project-name)
      (let* ((plist (cdr project))
             (base-dir (file-truename (expand-file-name (plist-get plist :base-directory))))
             (true-file (file-truename (expand-file-name file)))
             (root-pub-dir (file-name-as-directory (expand-file-name (plist-get plist :publishing-directory))))
             ;; Compute the subdir of the file relative to base-dir, then append to pub-dir
             (relative-dir (file-name-directory (file-relative-name true-file base-dir)))
             (pub-dir (if (and relative-dir (not (equal relative-dir "./")))
                          (expand-file-name relative-dir root-pub-dir)
                        root-pub-dir))
             (pub-fn (plist-get plist :publishing-function)))
        (when (and pub-fn
                   (string-prefix-p base-dir true-file))
          (message "Publishing %s -> %s (project: %s)" file pub-dir project-name)
          (make-directory pub-dir t)
          (funcall pub-fn plist file pub-dir)
          (org-publish-write-cache-file))))))

;; Publish only the changed file
(when (boundp 'changed-file)
  (message "Publishing single file: %s" changed-file)

  (dolist (project-name '("my-org-notes-articles"
                           "my-org-notes-main"
                           "my-org-notes-sitemap"
                           "my-private-notes-articles"
                           "my-private-notes-main"
                           "my-private-notes-sitemap"
                           "my-org-references-content"
                           "my-private-references-content"
                           "my-private-blog-content"))
    (my/publish-file-to-project changed-file project-name)))

(message "Incremental build complete!")
