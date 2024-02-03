;;; -*- lexical-binding: t; -*-


;; (setq doom-theme 'doom-feather-dark)
(setq doom-theme 'doom-outrun-electric)
;; (setq doom-theme 'doom-shades-of-purple)
;; (setq initial-frame-alist '((fullscreen . maximize)))

(setq
 org-agenda-files (append
                   (directory-files-recursively "~/Documents/Notes" "\\.org$")
                   (directory-files-recursively "~/Documents/OrgModeJupyter" "\\.org$"))
 org-directory "~/Documents/Notes"
 )
