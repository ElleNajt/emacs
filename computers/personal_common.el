;;; computers/personal_common.el -*- lexical-binding: t; -*-

;;; Gptel

(use-package! gptel)



;; (use-package! org-ai)
;; (setq org-ai-openai-api-token (shell-command-to-string "pass api-keys/openai-api"))
;; (setq  org-ai-image-directory "../images")

(defvar gptel-anthropic-initialized nil)
(after! gptel

  (setq gptel-use-curl t)
  (setq gptel-log-level 'debug)
  (setq gptel-include-reasoning t)

  ;; (unless gptel-anthropic-initialized
  ;;   (setq gptel-backend (gptel-make-anthropic "Claude"
  ;;                         :stream t 
  ;;                         :key (string-trim (shell-command-to-string "pass api-keys/claude-api"))
  ;;                         :models '(claude-3-7-sonnet-20250219)
  ;;                         :header (lambda () (when-let* ((key (gptel--get-api-key)))
  ;;                                              `(("x-api-key" . ,key)
  ;;                                                ("anthropic-version" . "2023-06-01")
  ;;                                                ("anthropic-beta" . "pdfs-2024-09-25")
  ;;                                                ("anthropic-beta" . "output-128k-2025-02-19")
  ;;                                                ("anthropic-beta" . "prompt-caching-2024-07-31"))))
  ;;                         :request-params '(:thinking (:type "enabled" :budget_tokens 2048)
  ;;                                           :max_tokens 4096)))
  ;;   (setq gptel-anthropic-initialized t))

  (setq gptel-log-level 'info)

  (defun get-anthropic-api-key () 
    "sometimes pass throws warnings, this ignores them"
    (let ((output (shell-command-to-string "pass api-keys/claude-api")))
      (if (string-match "\\(sk-ant[A-Za-z0-9_-]+\\)" output)
          (match-string 1 output)
        (error "Could not find API key starting with 'sk-ant' in the output"))))


  (unless gptel-anthropic-initialized
    (setenv "ANTHROPIC_API_KEY" (get-anthropic-api-key))
    (setq gptel-backend (gptel-make-anthropic "Claude"
                          :stream t 
                          :key (get-anthropic-api-key)))
    (setq gptel-anthropic-initialized t))

  (setq
   gptel-model 'claude-3-5-sonnet-20241022 ;  "claude-3-opus-20240229" also available


   ;; gptel-model 'claude-3-7-sonnet-20250219 ;  "claude-3-opus-20240229" also available
   ;; gptel-backend (gptel-make-openai "OpenAI"
   ;;                 :stream t :key (shell-command-to-string "pass api-keys/openai"))


   gptel-org-branching-context nil
   gptel-default-mode 'org-mode
   gptel-max-tokens 8192
   gptel-track-response t
   gptel-prompt-prefix-alist '((markdown-mode . "") (org-mode . "* USER\n") (text-mode . ""))
   gptel-response-prefix-alist '((markdown-mode . "") (org-mode . "* CLAUDE\n") (text-mode . ""))
   gptel-post-response-hook nil)


  (setq gptel-expert-commands nil)

  (setq gptel-stream t)

  ;; (add-hook 'gptel-post-response-functions
  ;;           (lambda (beg end)
  ;;             ;; (replace-stars-in-python-blocks)
  ;;             (goto-char (point-max))
  ;;             (newline)))


  (add-hook 'gptel-post-stream-hook
            (lambda ()
              (goto-char (point-max)))))

;; (defun replace-stars-in-python-blocks ()
;;   (interactive)
;;   (save-excursion
;;     (goto-char (point-max))
;;     (search-backward "* CLAUDE" nil t)
;;     (while (search-forward "#+begin_src python" nil t)
;;       (let ((start (point)))
;;         (when (search-forward "#+end_src" nil t)
;;           (save-restriction
;;             (narrow-to-region start (point))
;;             (goto-char start)
;;             (while (re-search-forward "^\\(\\s-*\\)\\*+" nil t)
;;               (replace-match "\\1#"))))))))

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

    (message "Defaults directory: %s" defaults-dir)
    (message "Checking files existence:")
    (message "shell.nix exists: %s" (file-exists-p shell-nix))
    (message "test.org exists: %s" (file-exists-p test-org))
    (message ".dir-locals.el exists: %s" (file-exists-p .dir-locals))
    (message ".envrc exists: %s" (file-exists-p .envrc))

    (make-directory project-dir)
    (message "Created project directory: %s" project-dir)

    (condition-case err
        (copy-file shell-nix (concat project-dir "shell.nix"))
      (error (message "Error copying shell.nix: %s" err)))

    (condition-case err
        (write-region (with-temp-buffer
                        (insert-file-contents test-org)
                        (replace-regexp-in-string "\\$CURRENT_DATETIME" current-datetime (buffer-string)))
                      nil
                      (concat project-dir "test.org"))
      (error (message "Error copying test.org: %s" err)))

    (condition-case err
        (copy-file .dir-locals (concat project-dir ".dir-locals.el"))
      (error (message "Error copying .dir-locals.el: %s" err)))

    (condition-case err
        (copy-file .envrc (concat project-dir ".envrc"))
      (error (message "Error copying .envrc: %s" err)))

    (message "Checking for additional files in defaults directory...")
    (dolist (file (directory-files defaults-dir t "^[^.]+"))
      (message "Found file: %s" file)
      (unless (member (file-name-nondirectory file) '("shell.nix" "test.org" ".dir-locals.el" ".envrc"))
        (condition-case err
            (copy-file file project-dir)
          (error (message "Error copying %s: %s" file err)))))

    (let ((default-directory project-dir))
      (message "Running direnv allow in %s" default-directory)
      (start-process "direnv" nil "direnv" "allow"))

    (message "Python project '%s' created successfully." project-name)))


;;; pdf tools


;;; password store
(use-package! exec-path-from-shell
  :config
  (progn
    (exec-path-from-shell-copy-env "PATH")
    (exec-path-from-shell-copy-env "PASSWORD_STORE_DIR")))

;;; python formatting

;; (use-package! python-black
;;   :after python
;;   :hook (python-mode . python-black-on-save-mode))

;; (use-package! python-isort
;;   :after python
;;   :hook (python-mode . python-isort-on-save-mode))

;;;  envrc stuff

(after! envrc
  (add-hook 'after-save-hook
            (lambda ()
              (when (string= (file-name-nondirectory buffer-file-name) ".envrc")
                (envrc-reload))))
  (add-hook 'projectile-after-switch-project-hook #'envrc-reload))

;;;  tramp

(after! tramp
  (set-default 'tramp-auto-save-directory "~/.emacs.d/.tramp-autosave")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))


;;; aider


(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  :custom
                                        ; See the Configuration section below
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet"))


;;; mcp

(load! (concat "mcp" ))
