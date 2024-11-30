;;; computers/personal_common.el -*- lexical-binding: t; -*-

;;; Gptel

(use-package! gptel
  :config  (setq! gptel-api-key (shell-command-to-string "pass api-keys/openai-api")))

(setq gptel-use-curl nil)
(setq gptel-log-level 'debug)

(map! (:nv "SPC o g g" 'gptel) )


;; (use-package! org-ai)
;; (setq org-ai-openai-api-token (shell-command-to-string "pass api-keys/openai-api"))
;; (setq  org-ai-image-directory "../images")

(setq
 gptel-model "claude-3-sonnet-20241022" ;  "claude-3-opus-20240229" also available
 gptel-backend (gptel-make-openai "OpenAI"
                 :stream t :key (shell-command-to-string "pass api-keys/openai"))
 gptel-backend (gptel-make-anthropic "Claude"
                 :stream t :key (shell-command-to-string "pass api-keys/claude-api"))
 gptel-default-mode 'org-mode
 gptel-track-response t
 gptel-prompt-prefix-alist '((markdown-mode . "") (org-mode . "* USER\n") (text-mode . ""))
 gptel-response-prefix-alist '((markdown-mode . "") (org-mode . "* CLAUDE\n") (text-mode . ""))
 ;; gptel-display-buffer-action
 gptel-rewrite-default-action 'ediff
 )

(setq gptel-post-stream-hook nil)

(add-hook 'gptel-post-response-functions
          (lambda (beg end)
            (goto-char (point-max))
            (newline)))

(add-hook 'gptel-post-stream-hook
          (lambda ()
            (message "Hook running!")
            (goto-char (point-max))
            (newline)))

;;; Ruff
(setq ruff-command "ruff check")

;;; Local keybindings:
(map! :mode python-mode
      (:nv "g RET" #'run/python))


(map! (:nv "g SPC" #'run/generic))

(map! :mode rustic-mode
      (:nv "g RET" #'rust/run)
      (:nv "g SPC" #'rust/check))

;;; Default python project

(defun new-python-project (project-name)
  "Create a new Python project folder with a specified structure in the current dired directory."
  (interactive "sProject name: ")
  (let* ((default-directory (dired-current-directory))
         (project-dir (concat default-directory project-name "/"))
         (defaults-dir (concat doom-user-dir "python/nix_project_defaults/"))
         (shell-nix (concat defaults-dir "shell.nix"))
         (test-org (concat defaults-dir "scratch.org"))
         (.dir-locals (concat defaults-dir ".dir-locals.el"))
         (.envrc (concat defaults-dir ".envrc"))
         (current-datetime (format-time-string "%Y-%m-%d %H:%M:%S")))
    (make-directory project-dir)
    (copy-file shell-nix (concat project-dir "shell.nix"))
    (write-region (with-temp-buffer
                    (insert-file-contents test-org)
                    (replace-regexp-in-string "\\$CURRENT_DATETIME" current-datetime (buffer-string)))
                  nil
                  (concat project-dir "test.org"))
    (copy-file .dir-locals (concat project-dir ".dir-locals.el"))
    (copy-file .envrc (concat project-dir ".envrc"))

    ;; Copy any other files from the defaults directory
    (dolist (file (directory-files defaults-dir t "^[^.]+"))
      (unless (member (file-name-nondirectory file) '("shell.nix" "test.org" ".dir-locals.el" ".envrc"))
        (copy-file file project-dir)))

    ;; Run direnv allow before moving into the directory
    (let ((default-directory project-dir))
      (shell-command "direnv allow"))

    (dired project-dir)
    (message "Python project '%s' created successfully." project-name)))

;;; pdf tools


;;; password store
(use-package! exec-path-from-shell
  :config
  (progn
    (exec-path-from-shell-copy-env "PATH")
    (exec-path-from-shell-copy-env "PASSWORD_STORE_DIR")))
