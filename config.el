;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;;; General stuff
(require 'general)
(general-evil-setup t)
(require 'predd)

(auto-save-visited-mode)
(evil-define-operator fireplace-eval (beg end)
  (pp-eval-expression (read (buffer-substring beg end))))

(nmap :keymaps 'emacs-lisp-mode-map
  "c" (general-key-dispatch 'evil-change
        "p" (general-key-dispatch 'fireplace-eval
              "p" 'eval-sexp-at-point
              "c" 'eval-last-sexp
              "d" 'eval-defun)))
;;;; Dired Stuff

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


;;;; Python
(use-package! python-black
  :demand t
  :after python)
(add-hook! 'python-mode-hook #'python-black-on-save-mode)
;; (map! :leader :desc "Blacken Buffer" "m b b" #'python-black-buffer)
;; (map! :leader :desc "Blacken Region" "m b r" #'python-black-region)
;; (map! :leader :desc "Blacken Statement" "m b s" #'python-black-statement)

;;;; Loading Other files
(load! (concat "computers/" (string-trim (shell-command-to-string "hostname"))))
(load! "configs/paxedit")
(load! "configs/orgbabelpython")
(load! "configs/orgmotions")
(load! "configs/magit")
(load! "configs/orgmode")
(load! (concat "computers/" (string-trim (shell-command-to-string "hostname")) "-after"))
(load! "configs/vendored/outli")


;;;; Keybindings
(map!
 :leader
 :desc "Shrink current window vertically" "w <down>" #'shrink-window
 :desc "Enlarge current window vertically" "w <up>" #'enlarge-window
 :desc "Shrink current window horizontally" "w <left>" #'shrink-window-horizontally
 :desc "Enlarge current window horizontally" "w <right>" #'enlarge-window-horizontally)
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
;; (remove-hook 'doom-first-input-hook #'evil-snipe-mode)


;;;; eww
(set-popup-rule! "^\\*eww\\*" :ignore t)
;;;; Flymake
(defun never-flymake-mode (orig &rest args)
  (when (and (bound-and-true-p flymake-mode))
    (funcall orig 0)
    (message "disabled flymake-mode")))
(advice-add #'flymake-mode :around #'never-flymake-mode)


;;;; evil motion trainer mode configurations

(global-evil-motion-trainer-mode 1)
(setq evil-motion-trainer-threshold 6)
;; (setq evil-motion-trainer-super-annoying-mode t)

(add-emt-advice evil-next-line
                '(evil-search-forward evil-jumper/backward evil-snipe-s)
                next-line)
(add-emt-advice evil-next-visual-line
                '(evil-search-forward evil-jumper/backward evil-snipe-s)
                next-line)
(add-emt-advice evil-previous-line
                '(evil-search-backward evil-snipe-S evil-jumper/backward evil-find-char-backward)
                previous-line)
(add-emt-advice evil-previous-visual-line
                '(evil-search-backward evil-snipe-S evil-jumper/backward evil-find-char-backward))
(add-emt-advice evil-forward-char
                '(evil-search-forward evil-find-char evil-snipe-f evil-snipe-s))
(add-emt-advice evil-backward-char
                '(evil-search-backward evil-find-char-backward evil-snipe-F evil-snipe-S))


(add-emt-advice evil-next-line
                '(evil-search-forward evil-jumper/backward evil-snipe-s)
                next-line)

;;;; Hot fuzz config
(use-package! hotfuzz
  :config (setq completion-styles '(hotfuzz)
                completion-ignore-case t))
;;;; Colors
(custom-set-faces!
  ;; `(hl-line :background "#000000")
  ;; `(default :background "#200000")
  ;; `(default :background "#200000")
  )

;;;; Outline
;;;;; Clean code folding via Outline minor mode.
;; (add-hook 'prog-mode-hook 'outline-minor-mode)
;; (add-hook 'text-mode-hook 'outline-minor-mode)
;;;;; Formatting it with outli
(add-hook 'prog-mode-hook 'outli-mode)
(add-hook 'text-mode-hook 'outli-mode)
;;;;; Keybindings

;; Tab to "zoom in" on a function, backtab to "zoom out" to the outline.
;; (evil-global-set-key 'normal (kbd "<tab>") 'outline-show-entry)
;; (evil-global-set-key 'normal (kbd "<backtab>") 'outline-hide-body)
;;
;; outli--on-heading

(map! (:mode outli-mode
       :n "TAB" #'outline-cycle
       :n "g j" 'outline-next-heading
       :n "g k" 'outline-previous-heading
       :n "g h" 'outline-up-heading))

;; (defcustom outli-speed-commands
;;   '(("Outline Navigation")
;;     ("n" . outline-next-visible-heading)
;;     ("p" . outline-previous-visible-heading)
;;     ("f" . outline-forward-same-level)
;;     ("b" . outline-backward-same-level)
;;     ("u" . outline-up-heading)
;;     ("Outline Visibility")
;;     ("c" . outline-cycle)
;;     ("C" . outline-cycle-buffer)
;;     ("s" . outli-toggle-narrow-to-subtree)
;;     ("h" . outline-hide-sublevels)
;;     ("1" . (outline-hide-sublevels 1))
;;     ("2" . (outline-hide-sublevels 2))
;;     ("3" . (outline-hide-sublevels 3))
;;     ("4" . (outline-hide-sublevels 4))
;;     ("5" . (outline-hide-sublevels 5))
;;     ("Outline Structure Editing")
;;     ("U" . outline-move-subtree-up)
;;     ("D" . outline-move-subtree-down)
;;     ("r" . outline-demote)
;;     ("l" . outline-promote)
;;     ("i" . outli-insert-heading-respect-content)
;;     ("@" . outline-mark-subtree)
;;     ("?" . outli-speed-command-help))
;;     z
