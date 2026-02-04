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

(defun org-google--push (format)
  "Push current org buffer to Google FORMAT (slides or doc).
Exports to ODT in Emacs, then runs Python upload async."
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))

  (let* ((org-file (buffer-file-name))
         (property (if (eq format 'slides) "GSLIDES_ID" "GDOC_ID"))
         (file-id (org-google--get-id property))
         (format-flag (if (eq format 'slides) "--slides" "--doc"))
         (odt-file (concat (file-name-sans-extension org-file) ".odt")))

    (save-buffer)
    (message "Exporting to ODT...")
    
    ;; Export to ODT (this respects #+OPTIONS:)
    (let ((org-confirm-babel-evaluate nil))
      (org-odt-export-to-odt))
    
    ;; Now run Python upload async (doesn't block Emacs)
    (message "Uploading to Google %s (async)..." (if (eq format 'slides) "Slides" "Docs"))
    
    ;; Clear the output buffer before starting
    (with-current-buffer (get-buffer-create "*org-google*")
      (erase-buffer))
    
    (let* ((cmd (if file-id
                    (format "%s %s upload %s %s --id %s"
                            (shell-quote-argument org-google-python-executable)
                            (shell-quote-argument org-google-python-script)
                            (shell-quote-argument odt-file)
                            format-flag
                            (shell-quote-argument file-id))
                  (format "%s %s upload %s %s"
                          (shell-quote-argument org-google-python-executable)
                          (shell-quote-argument org-google-python-script)
                          (shell-quote-argument odt-file)
                          format-flag)))
           (org-buf (current-buffer))
           ;; Capture these for the closure
           (captured-property property)
           (captured-odt-file odt-file)
           (proc (start-process-shell-command "org-google-upload" "*org-google*" cmd)))
      
      ;; Add filter to capture stdout
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
                   ;; Always save ID (updates create new files due to API limitation)
                   (when (buffer-live-p org-buf)
                     (with-current-buffer org-buf
                       (org-google--set-id captured-property new-id)))
                   ;; Extract and show URL
                   (when (string-match "URL:\\(.+\\)" output)
                     (let ((url (string-trim (match-string 1 output))))
                       (message "Pushed to: %s" url)
                       (browse-url url))))
               (message "Push failed: %s" output)))
           ;; Clean up ODT file
           (when (file-exists-p captured-odt-file)
             (delete-file captured-odt-file))))))))

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

;;;###autoload
(defun org-google-push-slides-new ()
  "Push to a NEW Google Slides presentation, ignoring existing ID."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))

  (let* ((org-file (buffer-file-name))
         (odt-file (concat (file-name-sans-extension org-file) ".odt")))

    (save-buffer)
    (message "Exporting to ODT...")

    ;; Export to ODT first
    (let ((org-confirm-babel-evaluate nil))
      (org-odt-export-to-odt))

    (message "Creating new Google Slides presentation...")

    (let* ((cmd (format "%s %s upload %s --slides"
                        (shell-quote-argument org-google-python-executable)
                        (shell-quote-argument org-google-python-script)
                        (shell-quote-argument odt-file)))
           (output (shell-command-to-string cmd)))
      (if (string-match "FILE_ID:\\(.+\\)" output)
          (let ((new-id (string-trim (match-string 1 output))))
            (when (y-or-n-p "Save new presentation ID to file? ")
              (org-google--set-id "GSLIDES_ID" new-id))

            (when (string-match "URL:\\(.+\\)" output)
              (let ((url (string-trim (match-string 1 output))))
                (message "Created: %s" url)
                (browse-url url))))
        (message "Push failed: %s" output))

      ;; Clean up ODT file
      (when (file-exists-p odt-file)
        (delete-file odt-file)))))

;;;###autoload
(defun org-google-push-doc-new ()
  "Push to a NEW Google Doc, ignoring existing ID."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))

  (let* ((org-file (buffer-file-name))
         (odt-file (concat (file-name-sans-extension org-file) ".odt")))

    (save-buffer)
    (message "Exporting to ODT...")

    ;; Export to ODT first
    (let ((org-confirm-babel-evaluate nil))
      (org-odt-export-to-odt))

    (message "Creating new Google Doc...")

    (let* ((cmd (format "%s %s upload %s --doc"
                        (shell-quote-argument org-google-python-executable)
                        (shell-quote-argument org-google-python-script)
                        (shell-quote-argument odt-file)))
           (output (shell-command-to-string cmd)))
      (if (string-match "FILE_ID:\\(.+\\)" output)
          (let ((new-id (string-trim (match-string 1 output))))
            (when (y-or-n-p "Save new document ID to file? ")
              (org-google--set-id "GDOC_ID" new-id))

            (when (string-match "URL:\\(.+\\)" output)
              (let ((url (string-trim (match-string 1 output))))
                (message "Created: %s" url)
                (browse-url url))))
        (message "Push failed: %s" output))

      ;; Clean up ODT file
      (when (file-exists-p odt-file)
        (delete-file odt-file)))))

(provide 'org-google)
;;; org-google.el ends here
