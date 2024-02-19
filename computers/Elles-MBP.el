;;; -*- lexical-binding: t; -*-


;; (setq doom-theme 'doom-feather-dark)
(setq doom-theme 'doom-outrun-electric)
;; (setq doom-theme 'doom-shades-of-purple)
;; (setq initial-frame-alist '((fullscreen . maximize)))

(setq
 org-agenda-files (append
                   (directory-files-recursively "~/Documents/Notes" "\\.org$")
                   (directory-files-recursively "~/Documents/OrgModeJupyter" "\\.org$"))
 org-directory "~/Documents/Notes")

;;;  Put my default python here?
;; (setq
;;  python-interpreter ""
;;  python-shell-interpreter ""
;;  )
;; (after! lsp-pyright
;;   (setq lsp-pyright-python-executable-cmd ""
;;         ))

(setq combobulate-source-code-path "~/Documents/GitHub/combobulate")
(setq tsfold-source-code-path "~/Documents/GitHub/ts-fold")
