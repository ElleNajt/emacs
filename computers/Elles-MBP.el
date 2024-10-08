;;; -*- lexical-binding: t; -*-

;;; Org file locations
(setq
 org-agenda-files (append
                   (directory-files-recursively "~/Documents/Notes" "\\.org$")
                   (directory-files-recursively "~/Documents/OrgModeJupyter" "\\.org$"))
 org-directory "~/Documents/Notes")

(after! org-mode
  (with-eval-after-load 'org
    (add-to-list 'org-capture-templates
                 '("f" "Food journal entry" entry
                   (file+olp+datetree  "~/Documents/Notes/foodjournal.org")
                   "* %U %?\n%i" :prepend t))

    (add-to-list 'org-agenda-custom-commands '("pa" "Aspen" tags-todo "aspen"))))

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

(load! (concat "personal_common" ))
;; turning off tree sitter globally
;; (load! "treesitter")
