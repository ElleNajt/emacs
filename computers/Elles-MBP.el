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


;;; Tidal cycles

;; (defun org-babel-execute:tidal (body params)
;;   "Execute a block of Tidal code with org-babel."
;;   (require 'tidal)
;;   (let ((session (cdr (assq :session params))))
;;     (when session
;;       (unless (and (get-process "tidal") (process-live-p (get-process "tidal")))
;;         (tidal-start-haskell))
;;       (tidal-send-string body))))

;; (eval-after-load 'org
;;   '(org-babel-do-load-languages
;;     'org-babel-load-languages
;;     '((tidal . t))))

;; (after! org
;;   (add-to-list 'org-babel-load-languages '(tidal . t))
;;   (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)

;;   (defun org-babel-execute:tidal (body params)
;;     (require 'tidal)
;;     (let ((session (cdr (assq :session params))))
;;       (when session
;;         (unless (and (get-process "tidal") (process-live-p (get-process "tidal")))
;;           (tidal-start-haskell))
;;         (tidal-send-string body)))))


(after! tidal
  (setq completion-at-point-functions
        (remove 'ispell-completion-at-point completion-at-point-functions))
  (add-hook 'org-mode-hook
            (lambda ()
              (remove-hook 'completion-at-point-functions #'ispell-completion-at-point t)))



  (map! :leader
        (:prefix-map ("l" . "tidal")
         ;; :desc "Start Tidal" "s" #'tidal-start-haskell
         :desc "Quit Tidal" "q" #'tidal-quit-haskell
         :desc "Hush" "h" #'tidal-hush
         :desc "Show output" "v" #'tidal-see-output
         :desc "Interrupt" "i" #'tidal-interrupt-haskell

         (:prefix ("e" . "eval")
          :desc "Eval line" "l" #'tidal-run-line
          :desc "Eval multi-line" "m" #'tidal-run-multiple-lines
          :desc "Eval region" "r" #'tidal-run-region
          :desc "Load buffer" "b" #'tidal-load-buffer)

         :desc "d1" "1" #'tidal-run-d1
         :desc "d2" "2" #'tidal-run-d2
         :desc "d3" "3" #'tidal-run-d3
         :desc "d4" "4" #'tidal-run-d4
         :desc "d5" "5" #'tidal-run-d5
         :desc "d6" "6" #'tidal-run-d6
         :desc "d7" "7" #'tidal-run-d7
         :desc "d8" "8" #'tidal-run-d8
         :desc "d9" "9" #'tidal-run-d9
         :desc "d10" "0" #'tidal-run-d10

         (:prefix ("s" . "stop")
          :desc "stop d1" "1" #'tidal-stop-d1
          :desc "stop d2" "2" #'tidal-stop-d2
          :desc "stop d3" "3" #'tidal-stop-d3
          :desc "stop d4" "4" #'tidal-stop-d4
          :desc "stop d5" "5" #'tidal-stop-d5
          :desc "stop d6" "6" #'tidal-stop-d6
          :desc "stop d7" "7" #'tidal-stop-d7
          :desc "stop d8" "8" #'tidal-stop-d8
          :desc "stop d9" "9" #'tidal-stop-d9
          :desc "stop d10" "0" #'tidal-stop-d10)))

  ;; (map! :map org-tidal-mode-map
  ;;       :localleader
  ;;       "h" #'tidal-hush
  ;;       "s" #'tidal-start-haskell
  ;;       "v" #'tidal-see-output
  ;;       "q" #'tidal-quit-haskell
  ;;       "c" #'tidal-run-line
  ;;       "e" #'tidal-run-multiple-lines
  ;;       "r" #'tidal-run-region
  ;;       "l" #'tidal-load-buffer
  ;;       "i" #'tidal-interrupt-haskell
  ;;       "m" #'tidal-run-main

  ;;       ;; Racks
  ;;       "1" #'tidal-run-d1
  ;;       "2" #'tidal-run-d2
  ;;       "3" #'tidal-run-d3
  ;;       "4" #'tidal-run-d4
  ;;       "5" #'tidal-run-d5
  ;;       "6" #'tidal-run-d6
  ;;       "7" #'tidal-run-d7
  ;;       "8" #'tidal-run-d8
  ;;       "9" #'tidal-run-d9
  ;;       "0" #'tidal-run-d10

  ;;       ;; Stop racks
  ;;       "!" #'tidal-stop-d1
  ;;       "@" #'tidal-stop-d2
  ;;       "#" #'tidal-stop-d3
  ;;       "$" #'tidal-stop-d4
  ;;       "%" #'tidal-stop-d5
  ;;       "^" #'tidal-stop-d6
  ;;       "&" #'tidal-stop-d7
  ;;       "*" #'tidal-stop-d8
  ;;       "(" #'tidal-stop-d9
  ;;       ")" #'tidal-stop-d10)

  (add-hook 'org-mode-hook 'org-tidal-mode))
