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
                     (browse-url url))))
             (message "Push failed: %s" output)))
         (when (file-exists-p upload-file)
           (delete-file upload-file)))))))

(defun org-google--push (format &optional force-new)
  "Push current org buffer to Google FORMAT (slides or doc).
For slides: converts org -> PPTX via pandoc, then uploads.
For docs: exports to ODT in background Doom batch, then uploads.
When FORCE-NEW is non-nil, create a new file even if an ID exists."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))

  (let* ((org-file (buffer-file-name))
         (base-name (file-name-sans-extension org-file))
         (property (if (eq format 'slides) "GSLIDES_ID" "GDOC_ID"))
         (file-id (unless force-new (org-google--get-id property)))
         (format-flag (if (eq format 'slides) "--slides" "--doc"))
         (upload-file (if (eq format 'slides)
                          (concat base-name ".pptx")
                        (concat base-name ".odt")))
         (org-buf (current-buffer)))

    (save-buffer)

    (if (eq format 'slides)
        ;; Slides: pandoc is fast, run synchronously then upload
        (progn
          (message "Converting to PPTX via pandoc...")
          (let* ((org-dir (file-name-directory org-file))
                 (temp-org (make-temp-file "org-google-" nil ".org"))
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

      ;; Docs: export to ODT in-process, then upload async
      (message "Exporting to ODT...")
      (let ((org-confirm-babel-evaluate nil)
            (process-environment (append '("MPLBACKEND=Agg") process-environment)))
        (org-odt-export-to-odt))
      (message "Uploading to Google Docs (async)...")
      (org-google--upload-async upload-file format-flag file-id org-buf property))))

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
