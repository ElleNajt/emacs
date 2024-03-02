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
;;  Links to code downloaded from git
;; (setq combobulate-source-code-path "~/Documents/GitHub/combobulate")
;; (setq tsfold-source-code-path "~/Documents/GitHub/ts-fold")

(load! "../vendored/combobulate-config")

;; (setq major-mode-remap-alist
;;       '((python-mode . python-ts-mode)))

(use-package! tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))
(use-package tree-sitter-langs)
;; (use-package ts-fold
;;   :load-path tsfold-source-code-path)

;; Setting for yabai:
(menu-bar-mode t)
