;;; -*- lexical-binding: t; -*-

(setq initial-frame-alist '((fullscreen . fullboth)))

(setq
 org-agenda-files (append
                   (directory-files-recursively "~/Documents/Notes" "\\.org$")
                   (directory-files-recursively "~/Documents/OrgModeJupyter" "\\.org$"))
 org-directory "~/Documents/Notes"
 )
