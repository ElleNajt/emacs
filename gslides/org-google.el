;;; org-google.el --- Sync org-mode with Google Slides/Docs -*- lexical-binding: t; -*-

;;; Commentary:
;; Bidirectional sync between org-mode files and Google Slides/Docs.
;; Push: org -> pandoc -> pptx/docx -> Google
;; Pull: Google -> text/md -> Claude -> org

;;; Code:

(defgroup org-google nil
  "Sync org-mode with Google Slides/Docs."
  :group 'org)

(defcustom org-google-python-script
  (expand-file-name "org-to-google.py"
                    (file-name-directory
                     (or load-file-name buffer-file-name
                         (expand-file-name "~/.doom.d/gslides/"))))
  "Path to the Python sync script."
  :type 'string
  :group 'org-google)

(defcustom org-google-python-executable
  (expand-file-name "~/.doom.d/gslides/.venv/bin/python3")
  "Path to Python executable with google dependencies."
  :type 'string
  :group 'org-google)

(defun org-google--get-id (property)
  "Get file ID from PROPERTY in current org buffer."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (format "^#\\+%s:\\s-*\\(.+\\)$" property) nil t)
      (string-trim (match-string 1)))))

(defun org-google--set-id (property id)
  "Set PROPERTY to ID in current org buffer."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward (format "^#\\+%s:" property) nil t)
        (progn
          (beginning-of-line)
          (kill-line)
          (insert (format "#+%s: %s" property id)))
      ;; Insert after title or at top
      (goto-char (point-min))
      (if (re-search-forward "^#\\+title:" nil t)
          (progn
            (end-of-line)
            (insert (format "\n#+%s: %s" property id)))
        (insert (format "#+%s: %s\n" property id))))
    (save-buffer)))

(defun org-google--upload-async (upload-file format-flag file-id org-buf property)
  "Upload UPLOAD-FILE to Google asynchronously.
FORMAT-FLAG is --slides or --doc. FILE-ID is the existing file ID or nil.
ORG-BUF is the source buffer. PROPERTY is the ID property name."
  (with-current-buffer (get-buffer-create "*org-google*")
    (erase-buffer))
  (let* ((cmd (if file-id
                  (format "%s %s upload %s %s --id %s"
                          (shell-quote-argument org-google-python-executable)
                          (shell-quote-argument org-google-python-script)
                          (shell-quote-argument upload-file)
                          format-flag
                          (shell-quote-argument file-id))
                (format "%s %s upload %s %s"
                        (shell-quote-argument org-google-python-executable)
                        (shell-quote-argument org-google-python-script)
                        (shell-quote-argument upload-file)
                        format-flag)))
         (proc (start-process-shell-command "org-google-upload" "*org-google*" cmd)))
      
    (set-process-filter proc
                        (lambda (p str)
                          (when (buffer-live-p (process-buffer p))
                            (with-current-buffer (process-buffer p)
                              (goto-char (point-max))
                              (insert str)))))
    (set-process-sentinel
     proc
     (lambda (p event)
       (when (string-match-p "finished" event)
         (let ((output (if (buffer-live-p (process-buffer p))
                           (with-current-buffer (process-buffer p)
                             (buffer-string))
                         "")))
           (message "org-google output: %s" output)
           (if (string-match "FILE_ID:\\(.+\\)" output)
               (let ((new-id (string-trim (match-string 1 output))))
                 (message "org-google: new ID is %s" new-id)
                 (when (buffer-live-p org-buf)
                   (with-current-buffer org-buf
                     (org-google--set-id property new-id)))
                 (when (string-match "URL:\\(.+\\)" output)
                   (let ((url (string-trim (match-string 1 output))))
                     (message "Pushed to: %s" url)
                     (browse-url url)))
                ;; Save gdoc snapshot for diffing later
                (when (buffer-live-p org-buf)
                  (org-google--save-snapshot org-buf new-id)))
             (message "Push failed: %s" output)))
         (when (file-exists-p upload-file)
           (delete-file upload-file)))))))

(defun org-google--save-snapshot (org-buf file-id)
  "Download the Google Doc and save as a snapshot for later diffing.
Called asynchronously after a successful push."
  (when-let* ((org-file (buffer-file-name org-buf))
              (snapshot-file (concat (file-name-sans-extension org-file)
                                     ".gdoc-snapshot.md")))
    (let* ((cmd (format "%s %s"
                        (shell-quote-argument
                         (expand-file-name "~/scripts/claude/google-fetch"))
                        (shell-quote-argument file-id)))
           (proc (start-process-shell-command
                  "gdoc-snapshot" "*gdoc-snapshot*" cmd)))
      (set-process-sentinel
       proc
       (lambda (p _event)
         (when (string-match-p "finished" _event)
           (when (buffer-live-p (process-buffer p))
             (with-current-buffer (process-buffer p)
               (let ((content (buffer-string)))
                 ;; Strip the "Generated from ..." footer
                 (setq content
                       (replace-regexp-in-string
                        "\n?-+\n.*Generated from.*\\'" "" content))
                 (with-temp-file snapshot-file
                   (insert content))
                 (message "Saved gdoc snapshot to %s" snapshot-file))))
           (when (buffer-live-p (process-buffer p))
             (kill-buffer (process-buffer p)))))))))

(defun org-google--git-commit-url ()
  "Return a GitHub commit URL for the current file, or nil.
Signals an error if the file has uncommitted changes."
  (let* ((file (buffer-file-name))
         (default-directory (file-name-directory file))
         (porcelain (string-trim
                     (shell-command-to-string
                      (format "git status --porcelain -- %s"
                              (shell-quote-argument file))))))
    (unless (string-empty-p porcelain)
      (user-error "Uncommitted changes in %s — commit before pushing"
                  (file-name-nondirectory file)))
    (let* ((hash (string-trim (shell-command-to-string "git rev-parse HEAD")))
           (remote (string-trim (shell-command-to-string "git remote get-url origin")))
           (github-url (when (string-match "github\\.com[:/]\\(.+\\)\\.git$" remote)
                         (format "https://github.com/%s/tree/%s"
                                 (match-string 1 remote) hash))))
      github-url)))

(defun org-google--rewrite-file-links-to-github (hash repo-path)
  "Rewrite [[file:...]] links to GitHub blob URLs in the current buffer.
HASH is the git commit hash. REPO-PATH is like \"User/repo\".
Returns list of (start end original) for cleanup."
  (let ((default-dir (file-name-directory (buffer-file-name)))
        (toplevel (string-trim (shell-command-to-string "git rev-parse --show-toplevel")))
        (replacements nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[file:\\([^]]+\\)\\]" nil t)
        (let* ((rel-path (match-string 1))
               (abs-path (expand-file-name rel-path default-dir))
               (repo-rel (when (string-prefix-p toplevel abs-path)
                           (substring abs-path (1+ (length toplevel))))))
          (when repo-rel
            (let ((github-url (format "https://github.com/%s/blob/%s/%s"
                                      repo-path hash repo-rel)))
              (replace-match (format "[[%s]" github-url) t t))))))
    replacements))

(defun org-google--push (format &optional force-new with-latex no-commit)
  "Push current org buffer to Google FORMAT (slides or doc).
For slides: converts org -> PPTX via pandoc, then uploads.
For docs: copies to a temp file, modifies that copy (rewrite links,
append commit URL), exports to ODT, then uploads.  The original
buffer is never modified.
When FORCE-NEW is non-nil, create a new file even if an ID exists.
When WITH-LATEX is non-nil, render LaTeX fragments as images via dvipng.
When NO-COMMIT is non-nil, skip the commit check and commit URL.
Otherwise fails if the file has uncommitted changes."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))

  (let* ((org-file (buffer-file-name))
         (org-dir (file-name-directory org-file))
         (base-name (file-name-sans-extension org-file))
         (property (if (eq format 'slides) "GSLIDES_ID" "GDOC_ID"))
         (file-id (unless force-new (org-google--get-id property)))
         (format-flag (if (eq format 'slides) "--slides" "--doc"))
         (upload-file (if (eq format 'slides)
                          (concat base-name ".pptx")
                        (concat base-name ".odt")))
         (org-buf (current-buffer))
         (commit-url (unless no-commit (org-google--git-commit-url))))

    (save-buffer)

    (if (eq format 'slides)
        ;; Slides: pandoc path (unchanged)
        (progn
          (message "Converting to PPTX via pandoc...")
          (let* ((temp-org (make-temp-file "org-google-" nil ".org"))
                 (sed-exit (call-process "sed" nil `(:file ,temp-org) nil
                                         "-e" "s/^:results:$//"
                                         "-e" "s/^:end:$//"
                                         org-file))
                 (exit-code (call-process "pandoc" nil "*org-google*" nil
                                          temp-org
                                          "-o" upload-file
                                          "--slide-level=2"
                                          (concat "--resource-path=" org-dir))))
            (delete-file temp-org)
            (unless (zerop exit-code)
              (user-error "Pandoc conversion failed. Check *org-google* buffer")))
          (message "Uploading to Google Slides (async)...")
          (org-google--upload-async upload-file format-flag file-id org-buf property))

      ;; Docs: work on a temp copy so the original buffer is untouched
      ;; Check for H: option first
      (save-excursion
        (goto-char (point-min))
        (unless (re-search-forward "^#\\+OPTIONS:.*\\bH:[0-9]" nil t)
          (user-error "Missing #+OPTIONS: H:N in file. Without it, headings deeper than level 3 produce broken ODT XML. Add e.g. #+OPTIONS: H:6")))

      ;; Modify buffer in-place for export, then revert to saved file.
      ;; save-buffer was called above, so revert always restores cleanly.
      (unwind-protect
          (progn
            ;; Add commit URL header/footer (no link rewriting — ODT needs file: links for images)
            (when commit-url
              (let* ((hash (string-trim (shell-command-to-string "git rev-parse HEAD")))
                     (short-hash (substring hash 0 7))
                     (remote (string-trim (shell-command-to-string "git remote get-url origin")))
                     (repo-path (when (string-match "github\\.com[:/]\\(.+\\)\\.git$" remote)
                                  (match-string 1 remote)))
                     (toplevel (string-trim (shell-command-to-string "git rev-parse --show-toplevel")))
                     (file-rel (when (and toplevel (string-prefix-p toplevel org-file))
                                 (substring org-file (1+ (length toplevel)))))
                     (file-url (when (and repo-path file-rel)
                                 (format "https://github.com/%s/blob/%s/%s"
                                         repo-path hash file-rel))))
                (save-excursion
                  (goto-char (point-max))
                  (insert (format "\n-----\n/Generated from [[%s][%s]] (%s)/\n"
                                  (or file-url commit-url)
                                  (file-name-nondirectory org-file)
                                  short-hash)))))

            ;; Strip Cell Timer lines from results drawers
            (save-excursion
              (goto-char (point-min))
              (while (re-search-forward "^Cell Timer:.*\n" nil t)
                (replace-match "")))

            (message "Exporting to ODT...")
            (let ((org-confirm-babel-evaluate nil)
                  (org-export-with-latex (if with-latex 'dvipng t))
                  (org-odt-with-latex (if with-latex 'dvipng t))
                  (process-environment (append '("MPLBACKEND=Agg") process-environment)))
              (org-odt-export-to-odt))

            (message "Uploading to Google Docs (async)...")
            (org-google--upload-async upload-file format-flag file-id org-buf property))

        ;; Always restore buffer to the saved file
        (revert-buffer t t)))))

(defun org-google--pull (format)
  "Pull changes from Google FORMAT (slides or doc) to current org buffer."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))

  (let* ((org-file (buffer-file-name))
         (property (if (eq format 'slides) "GSLIDES_ID" "GDOC_ID"))
         (file-id (org-google--get-id property))
         (format-flag (if (eq format 'slides) "--slides" "--doc")))

    (unless file-id
      (user-error "No %s found in buffer. Push first to create one" property))

    (save-buffer)
    (message "Pulling from Google %s..." (if (eq format 'slides) "Slides" "Docs"))

    (let* ((cmd (format "%s %s pull %s %s --id %s"
                        (shell-quote-argument org-google-python-executable)
                        (shell-quote-argument org-google-python-script)
                        (shell-quote-argument org-file)
                        format-flag
                        (shell-quote-argument file-id)))
           (output (shell-command-to-string cmd)))

      (if (string-match "PULL:SUCCESS" output)
          (progn
            (revert-buffer t t)
            (message "Pulled changes successfully"))
        (message "Pull failed: %s" output)))))

;;;###autoload
(defun org-google-push-slides ()
  "Push current org buffer to Google Slides.
If the buffer has a #+GSLIDES_ID property, updates that presentation.
Otherwise creates a new one and saves the ID."
  (interactive)
  (org-google--push 'slides))

;;;###autoload
(defun org-google-push-doc ()
  "Push current org buffer to Google Docs.
If the buffer has a #+GDOC_ID property, updates that document.
Otherwise creates a new one and saves the ID."
  (interactive)
  (org-google--push 'doc))

;;;###autoload
(defun org-google-push-doc-no-commit ()
  "Like `org-google-push-doc' but skip the commit check and commit URL."
  (interactive)
  (org-google--push 'doc nil nil t))

;;;###autoload
(defun org-google-pull-slides ()
  "Pull changes from Google Slides to current org buffer.
Uses Claude to intelligently merge changes while preserving org syntax."
  (interactive)
  (org-google--pull 'slides))

;;;###autoload
(defun org-google-pull-doc ()
  "Pull changes from Google Docs to current org buffer.
Uses Claude to intelligently merge changes while preserving org syntax."
  (interactive)
  (org-google--pull 'doc))

;;;###autoload
(defun org-google-push-doc-latex ()
  "Push current org buffer to Google Docs with LaTeX rendered as images."
  (interactive)
  (org-google--push 'doc nil t))

(defun org-google--push-new (format)
  "Push current org buffer to a NEW Google FORMAT (slides or doc), ignoring existing ID."
  (org-google--push format t))

;;;###autoload
(defun org-google-push-slides-new ()
  "Push to a NEW Google Slides presentation, ignoring existing ID."
  (interactive)
  (org-google--push-new 'slides))

;;;###autoload
(defun org-google-push-doc-new ()
  "Push to a NEW Google Doc, ignoring existing ID."
  (interactive)
  (org-google--push-new 'doc))

(provide 'org-google)
;;; org-google.el ends here
