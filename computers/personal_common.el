;;; computers/personal_common.el -*- lexical-binding: t; -*-

;;; Gptel

;; (use-package! gptel)

;; Load Claude CLI backend from patches
;; (load! (concat doom-user-dir "patches/gptel-claude-cli.el"))

;; (use-package! org-ai)
;; (setq org-ai-openai-api-token (shell-command-to-string "pass api-keys/openai-api"))
;; (setq  org-ai-image-directory "../images")

(defvar gptel-anthropic-initialized nil)
(defvar gptel-claude-cli-initialized nil)
(after! gptel

  (setq gptel-use-curl t)
  (setq gptel-log-level 'debug)
  (setq gptel-include-reasoning t)

  (setq gptel-log-level 'info)

  (defun get-anthropic-api-key ()
    "Read API key from ~/.config/anthropic/api_key"
    (let ((api-key-file (expand-file-name "~/.config/anthropic/api_key")))
      (if (file-exists-p api-key-file)
          (string-trim (with-temp-buffer
                         (insert-file-contents api-key-file)
                         (buffer-string)))
        (error "API key file not found: %s" api-key-file))))

  (unless gptel-anthropic-initialized
    (setenv "ANTHROPIC_API_KEY" (get-anthropic-api-key))
    (setq gptel-backend (gptel-make-anthropic "Claude"
                          :stream t
                          :key (get-anthropic-api-key)))
    (setq gptel-anthropic-initialized t))

  ;; Claude CLI backend setup (uses `claude -p` instead of API)
  ;; This uses your Claude subscription instead of API credits
  ;; Commented out - requires gptel-claude-cli patch
  ;; (unless gptel-claude-cli-initialized
  ;;   (gptel-make-claude-cli "Claude CLI"
  ;;     :model "claude-sonnet-4-5-20250929"
  ;;     :stream nil)  ; streaming not supported yet
  ;;   (setq gptel-backend (alist-get "Claude CLI" gptel--known-backends nil nil #'equal))
  ;;   (setq gptel-claude-cli-initialized t))


  ;; To switch back to Anthropic API backend:
  ;; M-x gptel-menu (C-c RET), then press 'b' to select backend, choose "Claude"

  (setq
   gptel-model 'claude-opus-4-20250514 ; Latest Opus 4 model (also available: 'claude-sonnet-4-20250514, 'claude-3-5-sonnet-20241022)
   gptel-org-branching-context nil
   gptel-default-mode 'org-mode
   gptel-max-tokens 8192
   gptel-track-response t
   gptel-prompt-prefix-alist '((markdown-mode . "") (org-mode . "* USER\n") (text-mode . ""))
   gptel-response-prefix-alist '((markdown-mode . "") (org-mode . "* CLAUDE\n") (text-mode . ""))
   gptel-post-response-hook nil)

  (setq gptel-expert-commands nil)
  (setq gptel-stream t)

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

           ;; Handle scratch.org with datetime and project name substitution
           ((string= item "scratch.org")
            (write-region (with-temp-buffer
                            (insert-file-contents source)
                            (let ((content (buffer-string)))
                              (setq content (replace-regexp-in-string "\\$CURRENT_DATETIME"
                                                                      current-datetime
                                                                      content))
                              (setq content (replace-regexp-in-string "\\$PROJECT_NAME"
                                                                      project-name
                                                                      content))
                              content))
                          nil dest)
            (message "Copied and processed: %s" item))

           ;; Handle regular files
           (t
            (copy-file source dest)
            (message "Copied file: %s" item))))))

    ;; Create venv with uv and install requirements in background
    (make-process
     :name "python-uv-setup"
     :command (list "sh" "-c" 
                    (format "cd %s && uv venv venv && uv pip install -r requirements.txt"
                            (shell-quote-argument project-dir)))
     :buffer nil
     :connection-type 'pipe
     :noquery t
     :sentinel (lambda (proc event)
                 (when (string-match-p "finished" event)
                   (message "UV setup completed, running direnv allow...")
                   ;; After venv and packages are installed, run direnv allow
                   (let ((default-directory project-dir))
                     (make-process
                      :name "direnv-allow"
                      :command (list "direnv" "allow" ".")
                      :buffer nil
                      :connection-type 'pipe
                      :noquery t
                      :sentinel (lambda (proc event)
                                  (when (string-match-p "finished" event)
                                    (message "direnv allow completed, initializing git...")
                                    ;; After direnv is configured, initialize git repo and commit
                                    (let ((default-directory project-dir))
                                      (make-process
                                       :name "git-init-commit"
                                       :command (list "sh" "-c" "git init && git add . && git commit -m 'Initial commit'")
                                       :buffer nil
                                       :connection-type 'pipe
                                       :noquery t
                                       :sentinel (lambda (proc event)
                                                   (when (string-match-p "finished" event)
                                                     (message "Python project '%s' created successfully with initial commit." project-name))))))))))

                 (message "Python project '%s' setup started..." project-name)))))


;;; pdf tools


;;; password store
(use-package! exec-path-from-shell
  :config
  (progn
    (exec-path-from-shell-copy-env "PATH")
    (exec-path-from-shell-copy-env "PASSWORD_STORE_DIR")
    (exec-path-from-shell-copy-env "HOME")))

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
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  
  ;; Performance improvements
  (setq remote-file-name-inhibit-cache nil)
  (setq tramp-verbose 1)  ; Reduce logging overhead
  
  ;; Reuse connections across programs (git, ls, etc.)
  (setq tramp-use-connection-share t)
  
  ;; Keep connections alive for 4 hours
  (setq tramp-connection-timeout 14400))

(defun tramp/afp-runpod ()
  "Connect to AFP RunPod cluster via TRAMP.
Uses SSH config entry 'afp-runpod' from ~/.ssh/config"
  (interactive)
  (find-file "/ssh:afp-runpod:/workspace-vast/lnajt/"))

(map! :leader
      :desc "TRAMP to AFP RunPod" "f a" #'tramp/afp-runpod)


;;; aider


(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  :custom
                                        ; See the Configuration section below
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "sonnet"))


;;; mcp

;; (load! (concat "mcp" ))

;;; RunPod management functions

(defun runpod/--get-repo-root ()
  "Get the git repository root, using current buffer's directory or default-directory."
  (let ((default-directory (if buffer-file-name
                               (file-name-directory buffer-file-name)
                             default-directory)))
    (vc-root-dir)))

(defun runpod/debug-env ()
  "Debug RunPod environment and config access."
  (interactive)
  (message "HOME: %s" (getenv "HOME"))
  (message "Config file exists: %s" (file-exists-p "~/.runpod/config.toml"))
  (message "Config file (expanded): %s" (file-exists-p (expand-file-name "~/.runpod/config.toml")))
  (let ((envrc-mode nil)
        (envrc-global-mode nil))
    (let ((output (shell-command-to-string "runpodctl get cloud 2>&1 | head -3")))
      (message "RunPodCTL output:\n%s" output))))

(defun runpod/update-config-from-clipboard ()
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

(defun runpod/provision ()
  "Provision a RunPod instance asynchronously and store pod_id in .runpod_config.json.
Uses GPU type and Docker image from config. Run runpod/configure first to set these.
Does NOT wait for SSH - use runpod/start to connect after provisioning."
  (interactive)
  (let* ((repo-root (runpod/--get-repo-root))
         (config-path (when repo-root
                        (concat (file-name-as-directory repo-root) ".runpod_config.json")))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))

    (unless repo-root
      (user-error "Not in a git repository"))

    (unless (file-exists-p config-path)
      (user-error "No .runpod_config.json found in repo root: %s" repo-root))

    (message "Reading config from: %s" config-path)

    (let* ((config (json-read-file config-path))
           (template-id (gethash "template_id" config))
           (gpu-type (gethash "gpu_type" config))
           (gpu-count (or (gethash "gpu_count" config) 1))
           (image-name (gethash "image_name" config))
           (project-name (file-name-nondirectory (directory-file-name repo-root))))

      ;; Validate required fields
      (unless (and gpu-type image-name)
        (user-error "Missing gpu_type or image_name in config. Run M-x runpod/configure first"))

      ;; Create the pod asynchronously (with envrc disabled)
      (let ((envrc-mode nil)
            (envrc-global-mode nil))
        (let* ((create-cmd
                (if template-id
                    ;; Template mode: still need gpuType and imageName even with template
                    (progn
                      (message "Provisioning RunPod using template %s: %dx %s (project: %s)"
                               template-id gpu-count gpu-type project-name)
                      (format "runpodctl create pod --templateId %s --gpuType %s --gpuCount %d --imageName %s --secureCloud --name %s 2>&1"
                              (shell-quote-argument template-id)
                              (shell-quote-argument gpu-type)
                              gpu-count
                              (shell-quote-argument image-name)
                              (shell-quote-argument project-name)))
                  ;; Custom mode: specify everything manually
                  (let ((container-disk (or (gethash "container_disk_size" config) 50))
                        (volume-size (or (gethash "volume_size" config) 100))
                        (ports (or (gethash "ports" config) "8888/http")))
                    (message "Provisioning RunPod: %dx %s (project: %s)" gpu-count gpu-type project-name)
                    (format "runpodctl create pod --gpuType %s --gpuCount %d --imageName %s --containerDiskSize %d --volumeSize %d --ports %s --secureCloud --name %s 2>&1"
                            (shell-quote-argument gpu-type)
                            gpu-count
                            (shell-quote-argument image-name)
                            container-disk
                            volume-size
                            (shell-quote-argument ports)
                            (shell-quote-argument project-name))))
                (output-buffer-name "*runpod-provision*"))

               ;; Kill any existing buffer with this name to avoid proliferation
               (when (get-buffer output-buffer-name)
                 (kill-buffer output-buffer-name))

               (message "Running command: %s" create-cmd)

               (make-process
                :name "runpod-provision"
                :buffer (get-buffer-create output-buffer-name)
                :command (list "sh" "-c" create-cmd)
                :connection-type 'pipe
                :noquery t
                :sentinel
                (lambda (process event)
                  (when (string-match-p "finished" event)
                    (with-current-buffer (process-buffer process)
                      (let ((create-output (buffer-string))
                            pod-id)
                        ;; Parse pod ID from output like: pod "726k31rc8izeox" created for $0.400 / hr
                        (if (string-match "pod \"\\([a-z0-9]+\\)\"" create-output)
                            (progn
                              (setq pod-id (match-string 1 create-output))
                              ;; Update config with pod_id only
                              (let ((json-object-type 'hash-table)
                                    (json-array-type 'list)
                                    (json-key-type 'string)
                                    (config (json-read-file config-path)))
                                (puthash "pod_id" pod-id config)
                                (with-temp-file config-path
                                  (insert (json-encode config))
                                  (json-pretty-print-buffer))
                                (message "RunPod provisioned! Pod ID: %s (use runpod/start to connect)" pod-id)))
                          (message "Failed to create pod or parse pod ID. Output:\n%s" create-output))))
                    (kill-buffer (process-buffer process)))))))))))

(defun runpod/start ()
  "Wait for SSH access to RunPod instance and update connection details.
Reads pod_id from .runpod_config.json and polls until SSH is available."
  (interactive)
  (let* ((repo-root (runpod/--get-repo-root))
         (config-path (when repo-root
                        (concat (file-name-as-directory repo-root) ".runpod_config.json")))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))

    (unless repo-root
      (user-error "Not in a git repository"))

    (unless (file-exists-p config-path)
      (user-error "No .runpod_config.json found in repo root: %s" repo-root))

    (let* ((config (json-read-file config-path))
           (pod-id (gethash "pod_id" config)))

      (unless pod-id
        (user-error "No pod_id found in config. Run runpod/provision first"))

      (message "Waiting for SSH access to pod %s..." pod-id)

      ;; Poll for pod to become available with SSH access
      (let ((envrc-mode nil)
            (envrc-global-mode nil))
        (let ((max-wait 180)  ; Increased to 3 minutes due to API slowness
              (wait-interval 10)  ; Check every 10 seconds instead of 5
              (elapsed 0)
              host port ssh-string)

          (while (and (< elapsed max-wait) (not host))
            (sleep-for wait-interval)
            (setq elapsed (+ elapsed wait-interval))

            (let* ((get-cmd (format "runpodctl get pod %s 2>&1" (shell-quote-argument pod-id)))
                   (get-output (shell-command-to-string get-cmd)))

              (message "Checking pod status... (%ds elapsed)" elapsed)

              ;; Look for SSH command in output: "ssh root@IP -p PORT"
              (when (string-match "ssh \\(?:root@\\)?\\([0-9.]+\\) -p \\([0-9]+\\)" get-output)
                (setq host (match-string 1 get-output)
                      port (match-string 2 get-output)
                      ssh-string (match-string 0 get-output)))))

          (unless host
            (user-error "Timeout waiting for pod to start. Check RunPod dashboard manually. Pod ID: %s" pod-id))

          ;; Update config with connection details
          (puthash "host" host config)
          (puthash "port" port config)

          ;; Write config back to file
          (with-temp-file config-path
            (insert (json-encode config))
            (json-pretty-print-buffer))

          (message "RunPod connected! SSH: %s" ssh-string))))))

(defun runpod/stop ()
  "Stop the RunPod instance and clear connection details.
Reads pod_id from .runpod_config.json, stops the pod, and clears host/port.
Keeps pod_id for potential restart. Use runpod/remove to fully delete."
  (interactive)
  (let* ((repo-root (runpod/--get-repo-root))
         (config-path (when repo-root
                        (concat (file-name-as-directory repo-root) ".runpod_config.json")))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))

    (unless repo-root
      (user-error "Not in a git repository"))

    (unless (file-exists-p config-path)
      (user-error "No .runpod_config.json found in repo root: %s" repo-root))

    (let* ((config (json-read-file config-path))
           (pod-id (gethash "pod_id" config)))

      (unless pod-id
        (user-error "No pod_id found in config. Has a pod been provisioned?"))

      (when (yes-or-no-p (format "Stop pod %s? " pod-id))
        (message "Stopping pod: %s" pod-id)

        ;; Run stop with envrc disabled
        (let ((envrc-mode nil)
              (envrc-global-mode nil))
          (let* ((stop-cmd (format "runpodctl stop pod %s 2>&1" (shell-quote-argument pod-id)))
                 (stop-output (shell-command-to-string stop-cmd)))
            (message "Stop output: %s" stop-output)))

        ;; Clear connection details but keep pod_id
        (puthash "host" "" config)
        (puthash "port" "" config)

        (with-temp-file config-path
          (insert (json-encode config))
          (json-pretty-print-buffer))

        (message "Pod %s stopped. Connection details cleared." pod-id)))))

(defun runpod/remove ()
  "Remove the RunPod instance completely and clear all config details.
Reads pod_id from .runpod_config.json, removes the pod, and clears all connection info."
  (interactive)
  (let* ((repo-root (runpod/--get-repo-root))
         (config-path (when repo-root
                        (concat (file-name-as-directory repo-root) ".runpod_config.json")))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))

    (unless repo-root
      (user-error "Not in a git repository"))

    (unless (file-exists-p config-path)
      (user-error "No .runpod_config.json found in repo root: %s" repo-root))

    (let* ((config (json-read-file config-path))
           (pod-id (gethash "pod_id" config)))

      (unless pod-id
        (user-error "No pod_id found in config. Nothing to remove."))

      (when (yes-or-no-p (format "Remove pod %s permanently? " pod-id))
        (message "Removing pod: %s" pod-id)

        ;; Run remove with envrc disabled
        (let ((envrc-mode nil)
              (envrc-global-mode nil))
          (let* ((remove-cmd (format "runpodctl remove pod %s 2>&1" (shell-quote-argument pod-id)))
                 (remove-output (shell-command-to-string remove-cmd)))
            (message "Remove output: %s" remove-output)))

        ;; Clear all connection details including pod_id
        (puthash "host" "" config)
        (puthash "port" "" config)
        (remhash "pod_id" config)

        (with-temp-file config-path
          (insert (json-encode config))
          (json-pretty-print-buffer))

        (message "Pod %s removed. Config cleared." pod-id)))))

(defun runpod/--parse-gpu-output (output)
  "Parse runpodctl get cloud output and return list of GPU specs."
  (let ((lines (split-string output "\n" t))
        gpus)
    ;; Skip header line and parse GPU entries
    (dolist (line (cdr lines))
      ;; Split by tabs and trim whitespace
      (let ((fields (mapcar #'string-trim (split-string line "\t" t))))
        (when (and (>= (length fields) 5)
                   (string-match "^[0-9]+x " (car fields)))
          (let* ((gpu-type-raw (nth 0 fields))
                 ;; Strip "1x " prefix for --gpuType parameter (e.g., "1x NVIDIA A40" -> "NVIDIA A40")
                 (gpu-type (replace-regexp-in-string "^[0-9]+x " "" gpu-type-raw))
                 (mem-gb (nth 1 fields))
                 (vcpu (nth 2 fields))
                 (spot-price (nth 3 fields))
                 (ondemand-price (nth 4 fields)))
            (push (list :type gpu-type
                        :type-display gpu-type-raw  ; Keep original for display
                        :mem mem-gb
                        :vcpu vcpu
                        :spot spot-price
                        :ondemand ondemand-price
                        :display (format "%-45s | %3sGB RAM | %2s vCPU | $%s/hr spot"
                                         gpu-type-raw mem-gb vcpu spot-price))
                  gpus)))))
    (nreverse gpus)))

(defun runpod/list-gpus (&optional callback)
  "List available RunPod GPU types with pricing information.
If CALLBACK is provided, runs asynchronously and calls callback with GPU list.
When called interactively, displays results in *RunPod GPUs* buffer."
  (interactive)
  (let ((display-fn
         (lambda (gpus)
           (let ((buf (get-buffer-create "*RunPod GPUs*")))
             (with-current-buffer buf
               (let ((inhibit-read-only t))
                 (erase-buffer)
                 (insert "Available RunPod GPUs\n")
                 (insert "====================\n\n")
                 (insert (format "%-45s | %8s | %6s | %12s | %12s\n"
                                 "GPU TYPE" "MEM GB" "VCPU" "SPOT $/HR" "ONDEMAND $/HR"))
                 (insert (make-string 100 ?-) "\n")
                 (if gpus
                     (dolist (gpu gpus)
                       (insert (format "%-45s | %8s | %6s | %12s | %12s\n"
                                       (plist-get gpu :type-display)
                                       (plist-get gpu :mem)
                                       (plist-get gpu :vcpu)
                                       (plist-get gpu :spot)
                                       (plist-get gpu :ondemand))))
                   (insert "\nNo GPUs found or API error.\n"))
                 (goto-char (point-min)))
               (view-mode 1))
             (display-buffer buf)
             (message "GPU list updated (%d types)" (length gpus))))))

    (if (called-interactively-p 'any)
        (progn
          (message "Fetching GPU list from RunPod (this may take 10-30 seconds)...")
          ;; Run async with envrc completely disabled
          (let ((envrc-mode nil)
                (envrc-global-mode nil))
            (make-process
             :name "runpod-get-cloud"
             :buffer "*runpod-get-cloud*"
             :command (list "runpodctl" "get" "cloud")
             :sentinel
             (lambda (process event)
               (when (string-match-p "finished" event)
                 (with-current-buffer (process-buffer process)
                   (let* ((output (buffer-string))
                          (gpus (runpod/--parse-gpu-output output)))
                     (funcall display-fn gpus)
                     (when callback
                       (funcall callback gpus)))))))))
      ;; Non-interactive: blocking call with envrc disabled
      (let ((envrc-mode nil)
            (envrc-global-mode nil))
        (let ((output (shell-command-to-string "runpodctl get cloud 2>&1")))
          (let ((gpus (runpod/--parse-gpu-output output)))
            (when callback
              (funcall callback gpus))
            gpus))))))

(defun runpod/select-gpu ()
  "Interactively select a GPU type and update default_machine in .runpod_config.json.
Prompts user to choose between using GPU type directly or entering a template ID."
  (interactive)
  (let* ((repo-root (runpod/--get-repo-root))
         (config-path (when repo-root
                        (concat (file-name-as-directory repo-root) ".runpod_config.json")))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string))

    (unless repo-root
      (user-error "Not in a git repository"))

    (unless (file-exists-p config-path)
      (user-error "No .runpod_config.json found in repo root: %s" repo-root))

    (let* ((gpus (runpod/list-gpus))
           (choices (mapcar (lambda (gpu) (plist-get gpu :display)) gpus))
           (selection (completing-read "Select GPU type: " choices nil t))
           (selected-gpu (cl-find-if (lambda (gpu)
                                       (string= (plist-get gpu :display) selection))
                                     gpus))
           (gpu-type (plist-get selected-gpu :type))
           (use-template (yes-or-no-p "Use template ID instead of GPU type? ")))

      (let ((machine-spec (if use-template
                              (read-string (format "Enter template ID for %s: " gpu-type))
                            gpu-type))
            (config (json-read-file config-path)))

        (puthash "default_machine" machine-spec config)

        (with-temp-file config-path
          (insert (json-encode config))
          (json-pretty-print-buffer))

        (message "Updated default_machine to: %s" machine-spec)
        (find-file-other-window config-path)))))

(defun runpod/configure ()
  "Interactively configure RunPod settings for the current project.
Select GPU type and optionally customize the Docker image."
  (interactive)
  (let* ((repo-root (runpod/--get-repo-root))
         (config-path (when repo-root
                        (concat (file-name-as-directory repo-root) ".runpod_config.json")))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (default-image "runpod/pytorch:2.8.0-py3.11-cuda12.8.1-cudnn-devel-ubuntu22.04"))

    (unless repo-root
      (user-error "Not in a git repository"))

    (unless (file-exists-p config-path)
      (user-error "No .runpod_config.json found in repo root: %s" repo-root))

    (message "Fetching GPU list from RunPod...")
    (let* ((config (json-read-file config-path))
           ;; Select GPU (blocking call for interactive config, envrc disabled)
           (gpus (let ((envrc-mode nil)
                       (envrc-global-mode nil))
                   (let ((output (shell-command-to-string "runpodctl get cloud 2>&1")))
                     (runpod/--parse-gpu-output output))))
           (gpu-choices (mapcar (lambda (gpu) (plist-get gpu :display)) gpus))
           (gpu-selection (completing-read "Select GPU type: " gpu-choices nil t))
           (selected-gpu (cl-find-if (lambda (gpu)
                                       (string= (plist-get gpu :display) gpu-selection))
                                     gpus))
           ;; Use :type which has "1x " prefix stripped, ready for --gpuType parameter
           (gpu-type (plist-get selected-gpu :type))

           ;; Image configuration
           (use-default-image (yes-or-no-p (format "Use default image (%s)? " default-image)))
           (image-name (if use-default-image
                           default-image
                         (read-string "Docker image name: " default-image)))

           ;; Storage settings
           (gpu-count (read-number "Number of GPUs: " 1))
           (container-disk (read-number "Container disk size (GB): " 50))
           (volume-size (read-number "Volume size (GB): " 100))
           ;; Note: SSH is provided automatically by RunPod, only specify additional ports here
           (ports (read-string "Ports to expose for web services (e.g., '8888/http' for Jupyter): " "8888/http")))

      ;; Update config
      (puthash "gpu_type" gpu-type config)
      (puthash "gpu_count" gpu-count config)
      (puthash "image_name" image-name config)
      (puthash "container_disk_size" container-disk config)
      (puthash "volume_size" volume-size config)
      (puthash "ports" ports config)

      ;; Save config
      (with-temp-file config-path
        (insert (json-encode config))
        (json-pretty-print-buffer))

      (message "RunPod configuration updated: %s on %s" gpu-type image-name)
      (find-file-other-window config-path))))

;;; macOS-specific paths for agent-shell

;; Add nix-profile binaries to exec-path so Emacs can find claudebox
(add-to-list 'exec-path "/Users/elle/.nix-profile/bin")
;; Use Node v20 for claude-code-acp compatibility (v23 has issues)
(add-to-list 'exec-path "/Users/elle/.nvm/versions/node/v20.19.5/bin")
;; Also update PATH environment variable for spawned processes
(setenv "PATH" (concat "/Users/elle/.nvm/versions/node/v20.19.5/bin:"
                       "/Users/elle/.nix-profile/bin:"
                       (getenv "PATH")))

(after! agent-shell 

  (setq agent-shell-anthropic-authentication
        (agent-shell-anthropic-make-authentication :api-key (get-anthropic-api-key)))
  
  ;; Set default model to Opus
  ;; It may be opus by default already?
  ;; (setq agent-shell-anthropic-default-model-id "claude-opus-4-20250514")

  )
