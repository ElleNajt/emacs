;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; This is because I was getting some weird magic-mode-alist settings
;; and this was breaking things. Couldn't figure out where it was set.
(setq magic-mode-alist nil)

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-shades-of-purple)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use  org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(defun dired-mode-p () (eq 'dired-mode major-mode))
(defun elle/dired-minus ()
  (interactive)
  (if (dired-mode-p)
      (dired-up-directory)
    (when buffer-file-name
      (-> (buffer-file-name)
          (f-dirname)
          (dired)))))
(map!
 :n "-" #'elle/dired-minus
 (:leader
  "b b" #'ido-switch-buffer
  "a" #'org-capture)
 (:mode emacs-lisp-mode
  :n   "g SPC" #'eval-buffer ))
(defun vterm-cd-to-dired-dir-and-switch ()
  "Change the current directory of the default vterm buffer (as opened with
`+vterm/toggle') to the directory of the current dired buffer, then switch to
it."
  (interactive)
  (let ((dir (dired-current-directory)))
    (if-let* ((projectile-vterm-buffer-name
               (format "*doom:vterm-popup:%s*"
                       (if (bound-and-true-p persp-mode)
                           (safe-persp-name (get-current-persp))
                         "main")))
              (vterm-buffer (get-buffer projectile-vterm-buffer-name)))
        (progn
          (pop-to-buffer vterm-buffer)
          (vterm-send-string (format "cd \"%s\"\n" dir)))
      (message "No currently open vterm (press SPC o t)"))))
(map!
 (:map dired-mode-map
       (:leader "d t" #'vterm-cd-to-dired-dir-and-switch)))

(require 'general)
(general-evil-setup t)
(require 'predd)

(evil-define-operator fireplace-eval (beg end)
  (pp-eval-expression (read (buffer-substring beg end))))

(nmap :keymaps 'emacs-lisp-mode-map
  "c" (general-key-dispatch 'evil-change
        "p" (general-key-dispatch 'fireplace-eval
              "p" 'eval-sexp-at-point
              "c" 'eval-last-sexp
              "d" 'eval-defun)))

(use-package! python-black
  :demand t
  :after python)
(add-hook! 'python-mode-hook #'python-black-on-save-mode)
;; (map! :leader :desc "Blacken Buffer" "m b b" #'python-black-buffer)
;; (map! :leader :desc "Blacken Region" "m b r" #'python-black-region)
;; (map! :leader :desc "Blacken Statement" "m b s" #'python-black-statement)
                                        ;
;; (visual-line-mode 1)
(load! (concat "computers/" (string-trim (shell-command-to-string "hostname"))))
(load! "paxedit")
(load! "orgbabelpython")
(load! "orgmode")
(load! (concat "computers/" (string-trim (shell-command-to-string "hostname")) "-after"))

(remove-hook 'doom-first-input-hook #'evil-snipe-mode)
(global-set-key (kbd "C-x <down>") 'shrink-window)
(global-set-key (kbd "C-x <up>") 'enlarge-window)
(global-set-key (kbd "C-x <right>") 'shrink-window-horizontally)
(global-set-key (kbd "C-x <left>") 'enlarge-window-horizontally)

(after! magit
  (transient-define-suffix magit-reset-head-back ()
    (interactive)
    (magit-reset-mixed "HEAD~"))
  (transient-define-suffix magit-reset-head-previous ()
    (interactive)
    (magit-reset-mixed "HEAD@{1}"))
  (transient-append-suffix
    #'magit-reset
    ["f"]
    (list "b" "Reset HEAD~"    #'magit-reset-head-back))
  (transient-append-suffix
    #'magit-reset
    ["f"]
    (list "o" "Reset HEAD@{1}" #'magit-reset-head-previous)))
