;;; -*- lexical-binding: t; -*-

;;; Theme
;; (setq doom-theme 'doom-feather-dark)
(setq doom-theme 'doom-outrun-electric)
;; (setq doom-theme 'doom-shades-of-purple)
;; (setq initial-frame-alist '((fullscreen . maximize)))
;;; Org file locations
(setq
 org-agenda-files (append
                   (directory-files-recursively "~/Documents/Notes" "\\.org$")
                   (directory-files-recursively "~/Documents/OrgModeJupyter" "\\.org$"))
 org-directory "~/Documents/Notes")

;;; Local only settings

;; Setting for yabai:
(menu-bar-mode t)
;;;; Shell settings (shell-command)
;;;;
;; (setq explicit-shell-file-name "bin/zsh")
(setq shell-file-name "/bin/zsh")

(use-package! exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-envs '("PATH" "MANPATH"))))
;;; Local keybindings:
(map! :mode python-mode
      (:nv "g RET" #'run/python))


(map! (:nv "g SPC" #'run/generic))


;;; Latex

(after! tex
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-command-extra-options "-shell-escape")
  (setq TeX-command-default "LatexMk")
  (setq TeX-save-query nil)
  (setq TeX-show-compilation t)
  (setq-default TeX-master nil))

(after! auctex
  (setq TeX-PDF-mode t))

;;;; Compile on save

;; Define a variable to control the auto-compilation
(defvar my-auto-compile-latex t
  "If non-nil, automatically compile LaTeX files on save.")

;; Define a function to toggle the auto-compilation
(defun my-toggle-auto-compile-latex ()
  "Toggle auto compilation of LaTeX files on save."
  (interactive)
  (setq my-auto-compile-latex (not my-auto-compile-latex))
  (message "Auto compile LaTeX on save: %s" (if my-auto-compile-latex "enabled" "disabled")))

;; Define a function to compile LaTeX if the toggle is enabled
(defun my-auto-compile-latex ()
  "Automatically compile LaTeX files if `my-auto-compile-latex` is non-nil."
  (when my-auto-compile-latex
    (TeX-command "LatexMk" 'TeX-master-file)))

;; Add the auto-compile function to the after-save-hook in LaTeX mode
(add-hook 'LaTeX-mode-hook
          (lambda ()
            (add-hook 'after-save-hook #'my-auto-compile-latex nil 'local)))

(map! :map LaTeX-mode-map
      :leader
      :desc "Toggle auto compile LaTeX"
      "t c" #'my-toggle-auto-compile-latex)
(setq ruff-command "ruff check")

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
