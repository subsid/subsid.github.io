;; Test suite for subsid.github.io build system
;; Run via: scripts/test.sh
;;
;; Uses ERT (Emacs built-in test framework).
;; Tests run against fixture files in test/fixtures/ rather than real notes.

(require 'ert)
(require 'package)

(setq package-user-dir (expand-file-name "./.packages"))
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa"  . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless (package-installed-p 'htmlize)
  (unless package-archive-contents (package-refresh-contents))
  (package-install 'htmlize))

(require 'ox-publish)

;; Load our helper functions WITHOUT running the full project setup.
;; We load build-site-config.el which also sets org-publish-project-alist,
;; but we override it below with fixture-based projects.
(load-file "./src/build-site-config.el")

;;; ---------------------------------------------------------------------------
;;; Test infrastructure
;;; ---------------------------------------------------------------------------

(defvar test/fixture-dir
  (expand-file-name "./test/fixtures/pages")
  "Root directory containing fixture org files.")

(defvar test/output-dir nil
  "Temporary output directory, created fresh per integration test.")

(defun test/make-output-dir ()
  "Create a fresh temp directory for test output and return its path."
  (let ((dir (make-temp-file "org-publish-test-" t)))
    (setq test/output-dir dir)
    dir))

(defun test/cleanup-output-dir ()
  "Delete the temp output directory after a test."
  (when (and test/output-dir (file-directory-p test/output-dir))
    (delete-directory test/output-dir t)
    (setq test/output-dir nil)))

(defun test/fixture (relative-path)
  "Return the absolute path to a fixture file at RELATIVE-PATH."
  (expand-file-name relative-path test/fixture-dir))

(defmacro test/with-output-dir (&rest body)
  "Run BODY with a fresh temp output dir, cleaning up afterwards."
  `(let ((test/output-dir (test/make-output-dir)))
     (unwind-protect
         (progn ,@body)
       (test/cleanup-output-dir))))

(defun test/make-project (name pub-dir &rest overrides)
  "Return a minimal org-publish project plist for testing.
NAME is the project name string, PUB-DIR is the output directory.
OVERRIDES are extra plist pairs appended after the defaults."
  `(,name
    :recursive t
    :base-directory ,test/fixture-dir
    :base-extension "org"
    :publishing-directory ,pub-dir
    :publishing-function my-conditional-html-publish
    :exclude ,(concat my-exclude-always "\\|" my-exclude-reference)
    ,@common-html-properties
    :sitemap-filename "notes.org"
    :sitemap-title "Notes"
    :sitemap-format-entry my-notes-sitemap-format-entry
    :sitemap-function (lambda (title list)
                        (create-sitemap-with-description
                         title "Test notes." list))
    ,@overrides))

(defun test/make-references-project (name pub-dir &rest overrides)
  "Return a references project plist for testing."
  `(,name
    :recursive t
    :base-directory ,test/fixture-dir
    :base-extension "org"
    :publishing-directory ,pub-dir
    :publishing-function my-conditional-html-publish
    :exclude ,(concat my-exclude-always "\\|" my-exclude-notes)
    ,@common-html-properties
    :sitemap-filename "references.org"
    :sitemap-title "References"
    :sitemap-format-entry my-notes-sitemap-format-entry
    :sitemap-function (lambda (title list)
                        (create-sitemap-with-description
                         title "Test references." list "useful-links"))
    ,@overrides))

;;; ---------------------------------------------------------------------------
;;; Unit tests: get-file-tags-info
;;; ---------------------------------------------------------------------------

(ert-deftest test/tags-published-article ()
  "get-file-tags-info returns has-publish=t for a file with #+TAGS: publish."
  (let ((info (get-file-tags-info (test/fixture "article/published-article.org"))))
    (should info)
    (should (nth 0 info))        ; has-publish
    (should-not (nth 1 info))    ; has-sitemapignore
    (should-not (nth 2 info))))  ; has-notitle

(ert-deftest test/tags-unpublished-article ()
  "get-file-tags-info returns has-publish=nil for a file without #+TAGS: publish."
  (let ((info (get-file-tags-info (test/fixture "article/unpublished-article.org"))))
    (should info)
    (should-not (nth 0 info))    ; has-publish
    (should-not (nth 1 info))    ; has-sitemapignore
    (should-not (nth 2 info))))  ; has-notitle

(ert-deftest test/tags-notitle-article ()
  "get-file-tags-info detects both publish and notitle tags."
  (let ((info (get-file-tags-info (test/fixture "article/notitle-article.org"))))
    (should info)
    (should (nth 0 info))        ; has-publish
    (should-not (nth 1 info))    ; has-sitemapignore
    (should (nth 2 info))))      ; has-notitle

(ert-deftest test/tags-sitemapignore-article ()
  "get-file-tags-info detects both publish and sitemapignore tags."
  (let ((info (get-file-tags-info (test/fixture "article/sitemapignore-article.org"))))
    (should info)
    (should (nth 0 info))        ; has-publish
    (should (nth 1 info))        ; has-sitemapignore
    (should-not (nth 2 info))))  ; has-notitle

;;; ---------------------------------------------------------------------------
;;; Unit tests: create-sitemap-entry
;;; ---------------------------------------------------------------------------

(ert-deftest test/sitemap-entry-uses-export-file-name ()
  "create-sitemap-entry respects #+EXPORT_FILE_NAME and preserves subdir."
  (let* ((project (test/make-project "test-proj" "/tmp/out"))
         (entry (test/make-project "test-proj" "/tmp/out")) ; unused, just need project form
         (result (create-sitemap-entry
                  "article/published-article.org"
                  (list "test-proj" :base-directory test/fixture-dir))))
    (should result)
    ;; Link should point to the EXPORT_FILE_NAME value, not the source filename
    (should (string-match-p "published-article\\.html" result))
    ;; Subdirectory must be preserved
    (should (string-match-p "article/" result))
    ;; Date should appear
    (should (string-match-p "2024-03-15" result))
    ;; Title should appear
    (should (string-match-p "Published Article" result))))

(ert-deftest test/sitemap-entry-without-export-file-name ()
  "create-sitemap-entry falls back to source filename when no EXPORT_FILE_NAME."
  ;; create-sitemap-entry calls org-publish-find-title which requires an
  ;; initialized cache, so we set one up in a temp dir first.
  (let* ((project-name "test-proj-cache")
         (org-publish-project-alist
          (list (test/make-project project-name "/tmp/unused-cache-dir")))
         (org-publish-timestamp-directory (make-temp-file "org-ts-" t)))
    (org-publish-initialize-cache project-name)
    (unwind-protect
        (let* ((result (create-sitemap-entry
                        "article/unpublished-article.org"
                        (list project-name :base-directory test/fixture-dir))))
          (should result)
          (should (string-match-p "unpublished-article\\.html" result))
          (should (string-match-p "article/" result)))
      (delete-directory org-publish-timestamp-directory t))))

(ert-deftest test/sitemap-entry-reference-export-file-name ()
  "create-sitemap-entry handles EXPORT_FILE_NAME in a reference subdir."
  (let* ((result (create-sitemap-entry
                  "reference/pinned-reference.org"
                  (list "test-proj" :base-directory test/fixture-dir))))
    (should result)
    (should (string-match-p "useful-links\\.html" result))
    (should (string-match-p "reference/" result))))

;;; ---------------------------------------------------------------------------
;;; Unit tests: create-sitemap-with-description (pinning)
;;; ---------------------------------------------------------------------------

(ert-deftest test/sitemap-pinned-entry-is-first ()
  "create-sitemap-with-description moves the pinned entry to the front."
  (let* ((entries '("[[file:reference/snippet.html][CLI Snippets]] /2024-02-20/"
                    "[[file:reference/useful-links.html][Links]] /2023-01-01/"))
         (result (create-sitemap-with-description "Refs" "Desc." entries "useful-links")))
    ;; useful-links should appear before snippet in output
    (should (< (string-match "useful-links" result)
               (string-match "snippet" result)))))

(ert-deftest test/sitemap-no-pin-preserves-order ()
  "create-sitemap-with-description without pinned arg preserves entry order."
  (let* ((entries '("[[file:article/published-article.html][Published]] /2024-03-15/"
                    "[[file:article/notitle-article.html][No Title]] /2024-06-01/"))
         (result (create-sitemap-with-description "Notes" "Desc." entries)))
    (should (< (string-match "published-article" result)
               (string-match "notitle-article" result)))))

(ert-deftest test/sitemap-pinned-missing-entry-leaves-order-unchanged ()
  "Pinned value that matches nothing leaves the entry order unchanged."
  (let* ((entries '("[[file:article/published-article.html][Published]] /2024-03-15/"
                    "[[file:article/notitle-article.html][No Title]] /2024-06-01/"))
         (result (create-sitemap-with-description "Notes" "Desc." entries "nonexistent")))
    (should (< (string-match "published-article" result)
               (string-match "notitle-article" result)))))

;;; ---------------------------------------------------------------------------
;;; Integration tests: full ox-publish run against fixtures
;;; ---------------------------------------------------------------------------

(ert-deftest test/integration-published-article-appears-in-public ()
  "Published article (#+TAGS: publish) is exported to the public output dir."
  (test/with-output-dir
   (let* ((pub-dir test/output-dir)
          (org-publish-project-alist
           (list (test/make-project "test-notes" pub-dir)))
          (org-publish-cache nil)
          (org-publish-timestamp-directory
           (expand-file-name "timestamps/" pub-dir)))
     (org-publish "test-notes" t)
     (should (file-exists-p
              (expand-file-name "article/published-article.html" pub-dir))))))

(ert-deftest test/integration-unpublished-article-absent-from-public ()
  "File without #+TAGS: publish is NOT exported to the public output dir."
  (test/with-output-dir
   (let* ((pub-dir test/output-dir)
          (org-publish-project-alist
           (list (test/make-project "test-notes" pub-dir)))
          (org-publish-cache nil)
          (org-publish-timestamp-directory
           (expand-file-name "timestamps/" pub-dir)))
     (org-publish "test-notes" t)
     (should-not (file-exists-p
                  (expand-file-name "article/unpublished-article.html" pub-dir))))))

(ert-deftest test/integration-notitle-suppresses-h1 ()
  "File with #+TAGS: notitle does not have an <h1> title in the exported HTML."
  (test/with-output-dir
   (let* ((pub-dir test/output-dir)
          (org-publish-project-alist
           (list (test/make-project "test-notes" pub-dir)))
          (org-publish-cache nil)
          (org-publish-timestamp-directory
           (expand-file-name "timestamps/" pub-dir)))
     (org-publish "test-notes" t)
     (let* ((html-file (expand-file-name "article/notitle-article.html" pub-dir))
            (content (with-temp-buffer
                       (insert-file-contents html-file)
                       (buffer-string))))
       (should (file-exists-p html-file))
       (should-not (string-match-p "<h1[^>]*>No Title Article</h1>" content))))))

(ert-deftest test/integration-sitemapignore-absent-from-sitemap ()
  "File with #+TAGS: sitemapignore is published as HTML but absent from notes.org.
ox-publish writes the sitemap org file into the base directory, not pub-dir."
  (test/with-output-dir
   (let* ((pub-dir test/output-dir)
          (org-publish-project-alist
           (list (test/make-project "test-notes" pub-dir)))
          (org-publish-cache nil)
          (org-publish-timestamp-directory
           (expand-file-name "timestamps/" pub-dir)))
     (unwind-protect
         (progn
           (org-publish "test-notes" t)
           ;; HTML should exist in pub-dir
           (should (file-exists-p
                    (expand-file-name "article/sitemapignore-article.html" pub-dir)))
           ;; Sitemap org is written to the base dir by ox-publish
           (let* ((sitemap-file (expand-file-name "notes.org" test/fixture-dir))
                  (content (with-temp-buffer
                             (insert-file-contents sitemap-file)
                             (buffer-string))))
             (should-not (string-match-p "sitemapignore-article" content))))
       ;; Clean up generated sitemap org from fixture dir
       (let ((sitemap (expand-file-name "notes.org" test/fixture-dir)))
         (when (file-exists-p sitemap) (delete-file sitemap)))))))

(ert-deftest test/integration-references-links-pinned-first ()
  "The Links/useful-links reference is the first entry in references.org.
ox-publish writes the sitemap org file into the base directory, not pub-dir."
  (test/with-output-dir
   (let* ((pub-dir test/output-dir)
          (org-publish-project-alist
           (list (test/make-references-project "test-refs" pub-dir)))
          (org-publish-cache nil)
          (org-publish-timestamp-directory
           (expand-file-name "timestamps/" pub-dir)))
     (unwind-protect
         (progn
           (org-publish "test-refs" t)
           ;; Sitemap org is written to the base dir by ox-publish
           (let* ((sitemap-file (expand-file-name "references.org" test/fixture-dir))
                  (content (with-temp-buffer
                             (insert-file-contents sitemap-file)
                             (buffer-string))))
             (should (< (string-match "useful-links" content)
                        (string-match "snippet" content)))))
       ;; Clean up generated sitemap org from fixture dir
       (let ((sitemap (expand-file-name "references.org" test/fixture-dir)))
         (when (file-exists-p sitemap) (delete-file sitemap)))))))

;;; ---------------------------------------------------------------------------
;;; Run all tests
;;; ---------------------------------------------------------------------------

(ert-run-tests-batch-and-exit)
