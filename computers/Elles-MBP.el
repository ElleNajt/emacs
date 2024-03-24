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

;;; Local keybindings:
(map! :mode python-mode
      (:nv "g RET" #'run/python))
