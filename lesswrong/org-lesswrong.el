;;; org-lesswrong.el --- Export org-mode to LessWrong-ready markdown -*- lexical-binding: t; -*-

;;; Commentary:
;; Export current org buffer to markdown via ox-md, then post-process
;; to replace local image paths with GitHub raw URLs, strip Cell Timer
;; lines, fix HTML entities, and append a git commit link.
;;
;; Usage: M-x org-lesswrong-export

;;; Code:

(defgroup org-lesswrong nil
  "Export org-mode to LessWrong-ready markdown."
  :group 'org)

(defcustom org-lesswrong-postprocess-script
  (expand-file-name "postprocess-md.py"
                    (file-name-directory
                     (or load-file-name buffer-file-name
                         (expand-file-name "~/.doom.d/lesswrong/"))))
  "Path to the Python post-processing script."
  :type 'string
  :group 'org-lesswrong)

;;;###autoload
(defun org-lesswrong-export ()
  "Export current org buffer to LessWrong-ready markdown.
Requires the file to be committed. Exports via ox-md, then
post-processes to rewrite image URLs, clean up artifacts,
and append a git commit link."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  (save-buffer)
  (let* ((org-file (buffer-file-name))
         (org-dir (file-name-directory org-file))
         (md-file (concat (file-name-sans-extension org-file) ".md"))
         (default-directory org-dir)
         ;; Check for uncommitted changes
         (porcelain (string-trim
                     (shell-command-to-string
                      (format "git status --porcelain -- %s"
                              (shell-quote-argument org-file))))))
    (unless (string-empty-p porcelain)
      (user-error "Uncommitted changes in %s — commit before exporting"
                  (file-name-nondirectory org-file)))
    ;; Export to markdown
    (message "Exporting to markdown via ox-md...")
    (org-export-to-file 'md md-file
      nil nil nil nil nil
      (lambda (outfile)
        ;; Post-process (appends git link too)
        (message "Post-processing for LessWrong...")
        (let ((exit-code (call-process "python3" nil "*org-lesswrong*" nil
                                       org-lesswrong-postprocess-script
                                       outfile org-file)))
          (if (zerop exit-code)
              (progn
                (with-temp-buffer
                  (insert-file-contents outfile)
                  (kill-new (buffer-string)))
                (message "LessWrong export complete (copied to clipboard): %s" outfile))
            (pop-to-buffer "*org-lesswrong*")
            (user-error "Post-processing failed")))))))

(provide 'org-lesswrong)
;;; org-lesswrong.el ends here
