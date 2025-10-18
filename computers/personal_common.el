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
  (let* ((current-dir (dired-current-directory))
         (project-dir (concat current-dir project-name "/"))
         (defaults-dir (concat doom-user-dir "python/project_defaults/"))
         (current-datetime (format-time-string "%Y-%m-%d %H:%M:%S")))

    (make-directory project-dir t)
    (message "Created directory: %s" project-dir)

    ;; Copy everything from defaults directory except .direnv
    (dolist (item (directory-files defaults-dir nil "^[^.]\\|^\\."))
      (unless (member item '("." ".." ".direnv"))
        (let ((source (concat defaults-dir item))
              (dest (concat project-dir item)))
          (cond
           ;; Handle directories
           ((file-directory-p source)
            (copy-directory source dest nil t t)
            (message "Copied directory: %s" item))

           ;; Handle scratch.org with datetime substitution
           ((string= item "scratch.org")
            (write-region (with-temp-buffer
                            (insert-file-contents source)
                            (replace-regexp-in-string "\\$CURRENT_DATETIME"
                                                     current-datetime
                                                     (buffer-string)))
                          nil dest)
            (message "Copied and processed: %s" item))

           ;; Handle regular files
           (t
            (copy-file source dest)
            (message "Copied file: %s" item))))))

    ;; Run direnv allow in background with correct directory
    (let ((default-directory project-dir))
      (make-process
       :name "direnv-allow"
       :command (list "direnv" "allow" ".")
       :buffer nil
       :connection-type 'pipe
       :noquery t
       :sentinel (lambda (proc event)
                   (when (string-match-p "finished" event)
                     (message "direnv allow completed")))))

    ;; Create venv in background
    (make-process
     :name "python-venv"
     :command (list "python" "-m" "venv" (concat project-dir "venv"))
     :buffer nil
     :connection-type 'pipe
     :noquery t
     :sentinel (lambda (proc event)
                 (when (string-match-p "finished" event)
                   (message "Python project '%s' created successfully." project-name))))

    (message "Python project '%s' setup started..." project-name))))


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

;;; RunPod config updater

(defun elle/update-runpod-config-from-clipboard ()
  "Parse SSH command from clipboard and update .runpod_config.json in current buffer.
Only updates host and port, preserving all other fields like ssh_key and remote_dir."
  (interactive)
  (let* ((clipboard (current-kill 0 t))
         (ssh-regex "ssh \\(?:root@\\)?\\([0-9.]+\\) -p \\([0-9]+\\)")
         host port)

    (unless (string-match ssh-regex clipboard)
      (user-error "Clipboard doesn't contain SSH command in expected format: ssh root@HOST -p PORT"))

    (setq host (match-string 1 clipboard)
          port (match-string 2 clipboard))

    (unless (derived-mode-p 'json-mode 'js-mode)
      (user-error "Current buffer is not in JSON mode"))

    (save-excursion
      (goto-char (point-min))
      (let* ((json-object-type 'hash-table)
             (json-array-type 'list)
             (json-key-type 'string)
             (config (json-read)))

        ;; Update only host and port
        (puthash "host" host config)
        (puthash "port" port config)

        ;; Replace buffer contents with updated JSON
        (erase-buffer)
        (insert (json-encode config))
        (json-pretty-print-buffer)
        (save-buffer)
        (message "Updated and saved RunPod config: host=%s port=%s" host port)))))
