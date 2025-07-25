;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;;; Requirements
(require 'general)
(require 'predd)
(require 'yasnippet)
;;; Evil
(general-evil-setup t)
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

;;; Theme
(setq doom-theme 'doom-outrun-electric)
(setq doom-theme 'doom-dark+)

(setq doom-theme 'modus-vivendi)

(map! :leader "h T" 'modus-themes-toggle)

;; (setq doom-theme 'doom-shades-of-purple)
;; (setq doom-theme 'doom-feather-dark)
;;; Evil
(after! org

  (map! :map evil-org-mode-map

        :vn "g j" #'evil-next-visual-line
        :vn "g k" #'evil-previous-visual-line
        ;;  This breaks M-x in org mode for some reason
        ;; :vin "C-[" #'org-evil-motion-backward-block-begin
        ;; :vin "C-]" #'org-evil-motion-forward-block-begin

        ))

;;; Programming Languages

(map!
 :n "] e" 'flycheck-next-error
 :n "[ e" 'flycheck-previous-error)

;;;; clojure


(set-popup-rule! "^\\*eglot-help" :size 0.4 :side 'bottom :select t :modeline t)

;;;;; keybindings
(nmap :keymaps 'clojure-mode-map

  "SPC m e b" 'cider-eval-buffer
  "SPC c l" 'cider-ns-reload
  "SPC c l" 'zprint-format-buffer


  "c" (general-key-dispatch 'evil-change
        "p" (general-key-dispatch 'cider-eval-region
              "p" 'cider-eval-sexp-at-point  ; cpp
              "c" 'cider-eval-last-sexp      ; cpc
              "d" 'cider-eval-defun-at-point)))       ; cpd

(map! :after clojure-mode
      :map clojure-mode-map
      "S-<return>" #'(lambda ()
                       (interactive)
                       (cider-eval-defun-at-point)
                       (end-of-defun)
                       (forward-sexp)))


(setq  evil-cleverparens-use-s-and-S nil)

(add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
(require 'evil-cleverparens-text-objects)


;;;;; org babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)))

(setq org-babel-clojure-backend 'cider)

;;;;; Formatting
(require 'reformatter)

(reformatter-define zprint-format
  :program "zprint"
  :lighter " ZPrint")

(add-hook 'clojure-mode-hook #'zprint-format-on-save-mode)


;; (after! eglot
;;   (setq eglot-stay-out-of '()  ; Allow eglot to provide more info
;;         eglot-extend-to-xref t))

;; Custom function that prefers CIDER docs but falls back to LSP
(defun my/preferred-clojure-docs ()
  "Use CIDER docs when available, fallback to LSP."
  (interactive)
  (save-excursion
    (if (and (bound-and-true-p cider-mode)
             (cider-connected-p))
        (cider-doc)
      (+lookup/documentation))))

(map! :after clojure-mode
      :map clojure-mode-map
      :n "K" #'my/preferred-clojure-docs)

(map! :map cider-repl-mode-map
      :i "<up>"   #'cider-repl-previous-input
      :i "<down>" #'cider-repl-next-input)


(add-hook 'clojure-mode-hook #'eglot-ensure)

;; (add-to-list 'eglot-server-programs '(clojure-mode . ("clojure-lsp")))


(after! 'eglot
  (add-to-list 'eglot-server-programs
               '((clojure-mode clojurescript-mode) . 
                 ("clojure-lsp"))))



;;;; c/c++

;; (add-to-list 'eglot-server-programs '((c-mode c++-mode) "clangd"))
;;;; nix

(with-eval-after-load 'eglot
  (dolist (mode '((nix-mode . ("nixd"))))
    (add-to-list 'eglot-server-programs mode)))

;;;; Elisp
(evil-define-operator fireplace-eval-elisp (beg end)
  (pp-eval-expression (read (buffer-substring beg end))))

(nmap :keymaps 'emacs-lisp-mode-map
  "c" (general-key-dispatch 'evil-change
        "p" (general-key-dispatch 'fireplace-eval-elisp
              "p" 'eval-sexp-at-point
              "c" 'eval-last-sexp
              "d" 'eval-defun)))

;; (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)


;;;;; Paxedit
(load! "vendored/paxedit")

;;;; Rust

;;;; Python
;; (elpy-enable)

(add-hook 'python-mode-hook #'flymake-mode)
(add-hook 'python-ts-mode-hook #'flymake-mode)

(after! apheleia
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

;; (require 'py-isort)
;; (add-hook 'before-save-hook 'py-isort-before-save)

;; (after! python
;;   (add-hook! 'python-mode-hook
;;     (add-hook #'before-save-hook #'py-isort-before-save nil t )))

(defun vterm-run-and-return (command)
  (let* ((buffer-name (concat "vterm-" (replace-regexp-in-string " " "-" command )))
         (buffer (get-buffer-create buffer-name)))
    (save-selected-window
      (save-mark-and-excursion
        (display-buffer
         buffer
         '((display-buffer-reuse-window
            display-buffer-in-side-window)
           (side . bottom)
           (slot . 0)
           (window-height . 0.3)))
        (with-current-buffer buffer
          (unless (derived-mode-p 'vterm-mode)
            (vterm-mode))
          (vterm-send-string command)
          (vterm-send-return))))))

(defun debug/python ()
  (interactive)
  (vterm-run-and-return (concat "nix-shell --run 'python -m pdb" buffer-file-name "'")))

(defun run/python ()
  (interactive)
  (vterm-run-and-return (concat "nix-shell --run 'python " buffer-file-name "'")))

;;;  rust
(defun rust/run ()
  (interactive)
  (save-buffer)
  (vterm-run-and-return (concat (format  "cd %s & clear & cargo run" (file-name-directory buffer-file-name)))))


(defun rust/check ()
  (interactive)
  (save-buffer)
  (vterm-run-and-return (concat (format  "cd %s & clear & cargo check" (file-name-directory buffer-file-name)))))


(defun run/generic ()
  "Make the current buffer file executable and run it in vterm."
  (interactive)
  (let ((file-name buffer-file-name))
    (if (not file-name)
        (message "Buffer is not visiting a file")
      (progn
        (vterm-run-and-return (concat "chmod +x " file-name))
        (vterm-run-and-return file-name)))))

;;;; java
(add-to-list 'auto-mode-alist '("\\.gm\\'" . groovy-mode))
;;; Dired

(defun dired-mode-p () (eq 'dired-mode major-mode))
(defun elle/dired-minus ()
  (interactive)
  (if (dired-mode-p)
      (dired-up-directory)
    (when buffer-file-name
      (-> (buffer-file-name)
          (f-dirname)
          (dired)))))

;; (use-package parinfer-rust-mode
;;   :hook emacs-lisp-mode
;;   :init
;;   (setq parinfer-rust-auto-download t))

(defun vterm-cd-to-dired-dir-and-switch ()
  "Change the current directory of the default vterm buffer (as opened with
`+vterm/toggle') to the directory of the current dired buffer, then switch to
it."
  (interactive)
  (let ((dir (if buffer-file-name  (file-name-directory buffer-file-name) ( dired-current-directory  )
                 )))
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
 (:map dired-mode-map                   ;; TODO[W6DKx0fHae] Why does this work?
       ;; SCHEDULED: <2025-05-06 Tue>
       (:leader "d t" #'vterm-cd-to-dired-dir-and-switch)))



;; (add-hook 'dired-mode-hook 'dired-hide-details-mode)

;;; Loading computer specific files
(load! (concat "computers/" (string-trim (shell-command-to-string "hostname"))))

(let ((host-config (concat "computers/" (string-trim (shell-command-to-string "hostname")))))
  (when (file-exists-p host-config)
    (load! host-config)))
;;; Org mode

(map! (:mode org-mode
       :n "g s" #'org-edit-special
       :n "] c" #'evil-next-flyspell-error
       :n "[ c" #'evil-prev-flyspell-error))

;;; Fix evil shift width in org files


;; this is gross, I could figure out what is setting it
;; (add-variable-watcher
;;  'evil-shift-width
;;  (lambda (symbol newval operation where)
;;    (message "evil-shift-width changed to %s in %s by %s" newval where operation)))
;; (remove-variable-watcher 'evil-shift-width)

;; (after! evil
;;   (setq-default evil-shift-width 4)
;;   (add-hook 'org-mode-hook (lambda () (setq-local evil-shift-width 4))))

(after! org
  (advice-add 'org-mode :after (lambda (&rest _) (setq-local evil-shift-width 4))))

;;; Org evil keybindings

;; Loading in org-evil functions with keybindings removed and setting my own
(load! "vendored/org-evil/org-evil-core")
(load! "vendored/org-evil/org-evil-commands")
(load! "vendored/org-evil/org-evil-motion")

(org-evil--define-key 'motion 'org-evil-motion-mode
                      "[[" 'org-evil-motion-backward-block-begin
                      "]]" 'org-evil-motion-forward-block-begin)

;; https://discourse.doomemacs.org/t/common-config-anti-patterns/119
(add-hook! 'org-mode-hook 'org-evil-mode)

(undefine-key! evil-motion-state-map "[ [" "] ]")

;; (map! :map evil-motion-state-map
;;       :n "[ s" nil
;;       :n "] s" nil)

(map! :map org-mode-map
      :n "[[" nil
      :n "]]" nil)

(map! (:mode org-mode
       :n "] r" #'org-babel-goto-src-block-results
       :n "[ s" 'org-evil-block-beginning-of-block
       :n "] s" 'org-evil-block-end-of-block))

(org-evil--define-key 'motion 'org-evil-block-mode
                      "[ s" 'org-evil-block-beginning-of-block
                      "] s" 'org-evil-block-end-of-block)

;;  Per org-evil comment
;; Have to loop through as it looks like the text objects
;; don't configure correctly when binding multiple states
;; at once.
(dolist (mode '(operator visual))
  (org-evil--define-key mode 'org-evil-block-mode
                        "ib" 'org-evil-block-inner-block
                        "ab" 'org-evil-block-a-block))


(map! (:mode org-mode
       :n "<up>" 'org-evil-motion-backward-heading
       :n "<down>" 'org-evil-motion-forward-heading))

;;;; Deduplicate Todos
(defun sort-deduplicate-todos ()
  "Sort TODOs by date, deduplicate exact copies."
  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char (point-min))
      ;; Sort TODOs by contents
      (org-sort-entries nil ?a)
      (org-sort-entries nil ?d)

      ;; Deduplicate exact copies
      (let ((seen-todos (make-hash-table :test 'equal))
            (current-todo ""))
        (while (re-search-forward org-heading-regexp nil t)
          (setq current-todo (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
          (if (gethash current-todo seen-todos)
              (delete-region (line-beginning-position) (1+ (line-end-position)))
            (puthash current-todo t seen-todos))))
      )))
;; TODO[6U95GKPqEi] Would be great to also get this to remove TODOs if the corresponding DONE exists.


;;;; General
(auto-save-visited-mode)
;; this is actually annoying -- should only make it apply to org mode files
;;;; Org Capture
(with-eval-after-load 'org
  (defun org-projects ()
    (cl-loop for (tag) in
             (org-global-tags-completion-table
              (directory-files-recursively org-directory "\\.org$"))
             when (s-starts-with-p "project__" tag)
             collect tag))

  (defun org-people ()
    (cl-loop for (tag) in
             (org-global-tags-completion-table
              (directory-files-recursively org-directory  "\\.org$"))
             when (s-starts-with-p "people__" tag)
             collect tag))


  (setq org-hierarchical-todo-statistics nil
        +org-capture-todo-file  (concat org-directory "/inbox.org")
        +org-capture-journal-file (concat org-directory "/journal.org")
        org-capture-templates '(("a" "Todo" entry
                                 (file +org-capture-todo-file)
                                 "* TODO %?\n%i%U" )
                                ("t" "Todo" entry
                                 (file +org-capture-todo-file)
                                 "* TODO %?\n%i%U" )
                                ("T" "Todo with link" entry
                                 (file +org-capture-todo-file)
                                 "* TODO %?\n%i\n%a\n%U" )  ; Changed order and spacing
                                ("n" "Inbox-Note" entry
                                 (file+headline +org-capture-todo-file  "Inbox Notes")
                                 "* %?\n%i%T" )

                                ("j" "Journal entry" entry
                                 (file+olp+datetree +org-capture-journal-file)
                                 "* %U %?\n%i" :prepend t)


                                ("J" "Journal entry with link" entry
                                 (file+olp+datetree +org-capture-journal-file)
                                 "* %U %?\n%i\n%a" :prepend t)  ; Removed redundant newline
                                )
        org-archive-location (concat org-directory "/trash::* from %s")
        org-todo-keywords '((sequence "TODO(t)" "ACTIVE(a)" "|" "DONE(d)" "RUNNING(r)")
                            (sequence "NEXT(n)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))
        org-log-done 't
        org-agenda-hide-tags-regexp "journal\\|tag2\\|tags3"
        org-agenda-custom-commands
        '(("i" "Inbox" tags "inbox")
          ("p" . "Person...")
          ))

  ;; (dolist (person (org-people))
  ;;   (add-to-list 'org-agenda-custom-commands
  ;;                (list
  ;;                 (concat "p" (substring person 0 1))
  ;;                 (capitalize person)
  ;;                 'tags-todo
  ;;                 person)))

  )
(with-eval-after-load 'org (add-to-list 'org-modules 'org-habit t))

;;;; Agenda
;;  From https://emacs.stackexchange.com/questions/52994/org-mode-agenda-show-list-of-tasks-done-in-the-past-and-not-those-clocked
(with-eval-after-load 'org-agenda
  (add-to-list 'org-agenda-custom-commands
               '("W" "Weekly review"
                 agenda ""
                 ((org-agenda-start-day "-14d")
                  (org-agenda-span 18)
                  (org-agenda-start-on-weekday 1)
                  (org-agenda-start-with-log-mode '(closed))
                  (org-agenda-skip-function
                   '(org-agenda-skip-entry-if 'notregexp "^\\*+ DONE "))
                  ))))
;;;; Cycling
;;  per https://docs.doomemacs.org/v21.12/modules/lang/org/#,code-1
(after! evil-org
  (remove-hook 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

;;;; Evil Motions


;;;; Org-Babel Python

(defun python-org-header ()
  (interactive)
  (let ((session-name (read-string "Name of session: ")))
    (end-of-line)
    (insert (format "\n\n* Default
 :PROPERTIES:
 :header-args: :results output :async t :session %s
 :END:" session-name))))

;; C-c C-k alread bound to something in org mode, we add advice to the function that its
;; bound to to interrupt the process if the cursor is in a source block
;; (define-advice org-kill-note-or-show-branches
;;     (:around (orig &rest args) interrupt-org-babel-session)
;;   (if (org-element-type-p (org-element-at-point) 'src-block)
;;       (interrupt-org-babel-session)
;;     (apply orig args)))





;;;;; For python editing in org files
;; (setq-default tab-width 2) -- TODO the version of this that actually works
;;;; Org-babel Nix
(add-hook 'org-mode  'org-nix-shell-mode)


;;; Outline mode
;;;;  Clean code folding via Outline minor mode.
(add-hook 'prog-mode-hook 'outline-minor-mode)
(add-hook 'text-mode-hook 'outline-minor-mode)
;; Pretty formatting it with outli
(add-hook 'prog-mode-hook 'outli-mode)
(add-hook 'text-mode-hook 'outli-mode)
;;; Magit
(evil-set-command-property 'smerge-next :repeat nil)
(evil-set-command-property 'smerge-prev :repeat nil)
;;; Doom Settings
;;;; Editing
(setq +evil-want-o/O-to-continue-comments nil)
;;;; Fonts

;; (setq doom-font (font-spec :family "Input Mono Narrow" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "Fira Sans") ; inherits `doom-font''s :size
;;       doom-unicode-font (font-spec :family "Input Mono Narrow" :size 12)
;;       doom-big-font (font-spec :family "Fira Mono" :size 19))
;;;; Windows
(setq windmove-wrap-around t)
;;; Misc
;;;; eww
(set-popup-rule! "^\\*eww\\*" :ignore t)

;;;; Flymake
(defun never-flymake-mode (orig &rest args)
  (when (and (bound-and-true-p flymake-mode))
    (funcall orig 0)
    (message "disabled flymake-mode")))
(advice-add #'flymake-mode :around #'never-flymake-mode)


;;;; Hot fuzz config
(use-package! hotfuzz
  :config (setq completion-styles '(hotfuzz)
                completion-ignore-case t))
;;;; Colors
;; (require 'rainbow-mode)
;; (add-hook 'prog-mode-hook 'rainbow-mode)
;; (custom-set-faces!
;;   `(region
;;     ;; :inherit lazy-highlight
;;     ;; :inherit nil
;;     :foreground "#919ad9"
;;     :distantforeground "#131033"
;;     :background "#1575b0"
;;     )
;;   )

;;; Keybindings
;;;; Python
(defun positional-to-keyword-regexp (beg end)
  (save-excursion
    (goto-char beg)
    (while (re-search-forward "\\(\\w+\\)\\([,\\)]\\)" (+ 10 end ) t)
      (replace-match "\\1=\\1\\2" nil nil)))
  )
;; TODO[uZkGGYEgen] This runs into an 'invalid search bound: wrong side of point" error, but works otherwise
(defun positional-to-keyword (&optional beg end)
  "Converts positional arguments to keyword arguments within the selected region."
  (interactive "r")
  (if (not (use-region-p))
      (let* ((region (evil-inner-paren))
             (beg (car region))
             (end (car  (cdr region)))
             )
        (positional-to-keyword-regexp beg end))
    (positional-to-keyword-regexp beg end)))

(defun python/next-string ()
  (interactive)
  (re-search-forward "[\\'\"]"))

(defun python/previous-string ()
  (interactive)
  (re-search-backward "[\\'\"]"))

(map! :mode python-mode
      :leader :nv "c =" #'positional-to-keyword
      :leader :n "r" #'eglot-rename
      :leader :n "e r" #'eglot-reconnect
      :leader :n "e e" #'eglot)

(map!
 ;;  3 as in #
 :desc "Next Comment" :nv "] 3" #'+evil/next-comment
 :desc "Previous Comment" :nv "[ 3" #'+evil/previous-comment

 :nv "] s" #'python/next-string
 :nv "[ s" #'python/previous-string
 )


;;;; Windows

(map!
 :leader
 :desc "Shrink current window vertically" "w <down>" #'shrink-window
 :desc "Enlarge current window vertically" "w <up>" #'enlarge-window
 :desc "Shrink current window horizontally" "w <left>" #'shrink-window-horizontally
 :desc "Enlarge current window horizontally" "w <right>" #'enlarge-window-horizontally
 ( :mode +popup-buffer-mode
         :desc "Pop up minibuffer" "g o" #'+popup/raise
         ))

;;;; Magit
(map!
 (:leader
  (:desc "previous Git conflict" :n "[ n" #'smerge-prev)
  (:desc "next Git conflict" :n "] n" #'smerge-next)
  (:desc "smerge" :prefix "m"
   :desc "Keep Current" :n "SPC" #'smerge-keep-current
   :desc "Keep All"     :n "a" #'smerge-keep-all
   :desc "Keep Upper"   :n "u" #'smerge-keep-upper
   :desc "Keep Lower"   :n "l" #'smerge-keep-lower)))

(after! magit
  (transient-define-suffix magit-reset-head-back ()
    (interactive)
    (magit-reset-mixed "HEAD~"))
  (transient-define-suffix magit-reset-head-previous ()
    (interactive)
    (magit-reset-mixed "HEAD@{1}"))
  (transient-append-suffix
    #'magit-reset
    "f"
    (list "b" "Reset HEAD~"    #'magit-reset-head-back))
  (transient-append-suffix
    #'magit-reset
    "f"
    (list "o" "Reset HEAD@{1}" #'magit-reset-head-previous)))

;;;; Outline
(map!
 :map outline-minor-mode-map
 (:prefix "z"
  :nv "TAB" #'outline-cycle
  ;; :nv "h s" 'outline-hide-sublevels
  ;; bind outline-show-body
  :nv "r" #'evil-open-folds
  :nv "s" 'outline-show-all
  :nv "h" 'outline-show-only-headings
  :nv "J" 'outline-forward-same-level
  :nv "K" 'outline-backward-same-level
  :nv "j" 'outline-next-heading
  :nv "k" 'outline-previous-heading
  :nv "n" 'outli-toggle-narrow-to-subtree  ; replacing evil-scroll-start-column
  ;; Leaving z N bound to doom/widen-indirectly-narrowed-buffer
  :nv "u" 'outline-up-heading
  ;; make condition using outli-on-heading-p
  :nv "," 'outline-promote
  :nv "." 'outline-demote
  :nv "m k" 'outline-move-subtree-up
  :nv "m j" 'outline-move-subtree-down
  :nv "i" 'outli-insert-heading-respect-content
  ;; :nv "1" '(outline-hide-sublevels 1)
  ;; :nv "2" '(outline-hide-sublevels 2)
  ;; :nv "3" '(outline-hide-sublevels 3)
  ;; :nv "4" '(outline-hide-sublevels 4)
  ;; :nv "5" '(outline-hide-sublevels 5)
  ))

;;; Org mode
(defun org-time-stamp-advice (orig-fun &rest args)
  "Ensure a space is inserted before the Org timestamp."
  (unless (or (bolp) (eq (char-before) ?\s))
    (insert " "))
  (apply orig-fun args))

(advice-add 'org-time-stamp :around #'org-time-stamp-advice)

;;;; Org python

(map!
 (:mode org-agenda-mode
        "SPC m A" #'org-archive-subtree))
;;;; General
(map!
 :n "-" #'elle/dired-minus
 :n "g r" #'+lookup/references          ; previously +eval:region
 :n "g R" #'+lookup/references
 :n "SPC r" #'eglot-rename
 (:leader
  "b b" #'ido-switch-buffer
  "a" #'org-capture)
 (:mode emacs-lisp-mode
  :n   "g SPC" #'eval-buffer
  :n   "g RET" #'eval-buffer
  ))

;;;; Smooth scrolling

(add-hook 'org-mode-hook (lambda () (pixel-scroll-precision-mode 1)))

(setq pixel-scroll-precision-large-scroll-height 40.0)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time

(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling

(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-step 1) ;; keyboard scroll one line at a time


;;; Corfu

;; already in +evil-bindings
(after! corfu
  (setq corfu-popupinfo-delay 0)
  (setq corfu-auto t)
  (setq corfu-preselect t)
  )


;;   ;; (setq! corfu-preview-current nil
;;   ;;        corfu-preselect 'first
;;   ;;        completion-styles '(orderless basic partial-completion))

;; corfu key-bindings
;; (setq tab-always-indent t)
;; (map! :map corfu-map
;;       ;; :gi "TAB" nil
;;       ;; #'corfu-complete
;;       ;; :gi "<tab>" nil
;;       ;; #'corfu-complete
;;       :gi "C-y" #'corfu-complete
;;       "C-d" #'corfu-info-location
;;       "C-h" #'corfu-info-documentation)

;;; Colors for tags

(after! org
  (setq org-todo-keyword-faces
        '(("TODO" . (:foreground "#e61f44" :weight ultra-bold :box t :background "black" ))
          ("DONE" . (:foreground "#a7da1e" :weight bold :box t ))
          ("WAITING" . (:foreground "orange" :weight bold)))))

;;; Cancel  /mark done recursive


(defun cancel-all-in-org-file-or-subtree ()
  "Cancel all todos in the entire Org file if the cursor is on the #+title: line. Otherwise, cancel todos in the current subtree."
  (interactive)
  (save-excursion
    (let ((org-enforce-todo-dependencies nil)
          (is-on-title-line (save-excursion
                              (beginning-of-line)
                              (looking-at-p "#\\+title:"))))
      ;; Determine the scope of the search
      (if (not is-on-title-line)
          (org-narrow-to-subtree))

      ;; Perform the search and cancel TODOs
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ \\(TODO\\) " nil t)
        (org-todo "CANCELLED"))

      ;; Widen the buffer if it was narrowed
      (when (not is-on-title-line)
        (widen)))))

(defun mark-done-all-in-subtree ()
  "Cancel all todos in subtree."
  (interactive)
  (save-excursion
    (let ((org-enforce-todo-dependencies nil))
      (org-narrow-to-subtree)
      (goto-char ( point-min ))
      (while (search-forward "TODO" nil t)
        (org-todo "DONE"))
      (widen))))


(map! :after org
      :map org-mode-map
      :leader
      (:prefix ("o" . "custom")
       :desc "Cancel all TODOs in subtree or file" "C" #'cancel-all-in-org-file-or-subtree))



;;; Get directory as text


(defun doom/directory-as-text_all ()
  "Concatenate all text files in the current directory, separated by filenames."
  (interactive)
  (let ((output-buffer (get-buffer-create "*Concatenated Text*")))
    (with-current-buffer output-buffer
      (read-only-mode 0)  ; Ensure we can modify the buffer
      (erase-buffer)      ; Clear previous contents
      (shell-command (concat "bash " doom-user-dir "bashscripts/directoryastext.sh --all" ) output-buffer)
      (read-only-mode 1)) ; Make the buffer read-only again
    (display-buffer output-buffer)))

(defun doom/directory-as-text ()
  "Concatenate all text files in the current directory, separated by filenames."
  (interactive)
  (let ((output-buffer (get-buffer-create "*Concatenated Text*")))
    (with-current-buffer output-buffer
      (read-only-mode 0)  ; Ensure we can modify the buffer
      (erase-buffer)      ; Clear previous contents
      (shell-command (concat "bash " doom-user-dir "bashscripts/directoryastext.sh" ) output-buffer)
      (read-only-mode 1)) ; Make the buffer read-only again
    (display-buffer output-buffer)))

;; Optionally, bind this function to a key in dired-mode
(map! :after dired
      :map dired-mode-map
      "C-c c" #'doom/directory-as-text
      :n "L" #'dired-do-symlink

      :n "W" #'dired-do-eww)
(map!

 :n "SPC d a" #'envrc-allow
 :n "SPC d r" #'envrc-reload
 :n "SPC d R" #'envrc-reload-all

 )

;;; i3 status stuff


(defun elle/org-text-element->string (elt)
  (cond
   ((stringp elt) elt)
   ((and (consp elt)
         (symbolp (car elt)))
    (-> elt (caddr) (elle/org-text-element->string) (s-trim) (concat " ")))))

(defun elle/org-element-title (elt)
  (let ((title (org-element-property :title elt)))
    (cond
     ((stringp title) title)
     ((listp title)
      (->> title
           (mapcar #'elle/org-text-element->string)
           (s-join "")
           (s-trim))))))

(defun elle/minutes->hours:minutes (minutes)
  (format "%d:%02d"
          (floor (/ minutes 60))
          (mod minutes 60)))
(defun elle/org-element-clocked-in-task ()
  (elle/at-org-clocked-in-item
   (org-element-at-point)))


(defmacro elle/at-org-clocked-in-item (&rest body)
  `(when (org-clocking-p)
     (let ((m org-clock-marker))
       (with-current-buffer (marker-buffer m)
         (save-mark-and-excursion
           (goto-char m)
           (org-back-to-heading t)
           ,@body)))))

(defun elle/org-current-clocked-in-task-message ()
  (interactive)
  (if (org-clocking-p)
      (format "(%s) [%s]"
              (->> (elle/org-element-clocked-in-task)
                   (elle/org-element-title)
                   (substring-no-properties)
                   (s-trim))
              (elle/minutes->hours:minutes
               (org-clock-get-clocked-time)))
    ""))

(defun update-org-clocked-in-task-file ()
  (interactive)
  (let ((current-task (elle/org-current-clocked-in-task-message)))
    (with-temp-file "~/.emacs.d/current-task"
      (insert current-task))))

(add-hook 'org-clock-in-hook 'update-org-clocked-in-task-file)
(add-hook 'org-clock-out-hook 'update-org-clocked-in-task-file)
(add-hook 'org-after-todo-state-change-hook 'update-org-clocked-in-task-file)

(run-at-time "1 min" 60 'update-org-clocked-in-task-file)

(defun elle/test_message ()
  (interactive)
  "Hello from emacs!"
  )

;;;
;;;

;; (use-package eglot
;;   :config
;;   ;; Ensure `nil` is in your PATH.
;;   (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
;;   :hook
;;   (nix-mode . eglot-ensure))

;;; next action
;; idea from here: https://www.adventuresinwhy.com/post/next-actions/
(defun org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (interactive)
  (let (should-skip-entry)
    (unless (org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      ;; If previous sibling exists and is TODO,
      ;; skip this entry
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (org-current-is-todo)
          (setq should-skip-entry t))))
    (let ((num-ancestors (org-current-level))
          (ancestor-level 1))
      (while (and (not should-skip-entry) (<= ancestor-level num-ancestors))
        (save-excursion
          ;; When ancestor (parent, grandparent, etc) exists
          (when (ignore-errors (outline-up-heading ancestor-level t))
            ;; If ancestor is WAITING, skip entry
            (if (string= "WAITING" (org-get-todo-state))
                (setq should-skip-entry t)
              ;; Else if ancestor is TODO, check previous siblings of
              ;; ancestor ("uncles"); if any of them are TODO, skip
              (when (org-current-is-todo)
                (while (and (not should-skip-entry) (org-goto-sibling t))
                  (when (org-current-is-todo)
                    (setq should-skip-entry t)))))))
        (setq ancestor-level (1+ ancestor-level))
        ))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

;;; Turning off tree sitter

(setq tree-sitter-mode nil)

;;; Latex
(after! latex
  (setq TeX-auto-save t
        TeX-parse-self t
        ;; TeX-syntactic-comment t
        ;; Synctex support
        TeX-source-correlate-start-server nil
        ;; Don't insert line-break at inline math
        LaTeX-fill-break-at-separators nil)
  (setq-default TeX-master nil)

  ;; Enable nice osx pdf viewer
  (when (eq system-type 'darwin)
    (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
    (setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))))

(use-package! pdf-tools
  :config
  ;; (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t))

;; For inline LaTeX preview in Org mode
(after! org
  (setq org-startup-with-latex-preview t)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale .9))
  (setq org-latex-create-formula-image-program 'dvipng)


  (add-hook 'org-mode-hook 'org-fragtog-mode)
  (add-hook 'org-mode-hook 'org-toggle-pretty-entities)
  (add-hook 'latex-mode 'prettify-symbols-mode)


  (map!
   :map org-mode-map
   :ni "C-l" #'cdlatex-tab)

  (setq org-preview-latex-default-process 'dvipng)

  ;; Optional: Increase the size of the LaTeX fragment cache to reduce re-rendering
  (setq org-preview-latex-image-cache-max 200)  ; Default is 20

  ;; Optional: Set a directory for LaTeX preview images
  (setq org-preview-latex-image-directory "~/.emacs.d/.local/lt-cache/")
  )

;; If you're using the preview-latex package
(after! preview
  (setq-default preview-scale 1))  ; Adjust this value as needed

;; Load org-fragtog package
(use-package! org-fragtog
  :after org
  :hook (org-mode . org-fragtog-mode))


(add-hook 'org-mode-hook 'org-latex-preview)
;; (remove-hook 'org-mode-hook 'turn-on-org-cdlatex)
;; (remove-hook 'org-mode-hook 'turn-on-org-cdlatex)


(after! cdlatex
  (setq cdlatex-math-symbol-alist '()))

;;; based pyright

;; (after! eglot
;;   (add-to-list 'eglot-server-programs
;;                '((python-mode python-ts-mode)
;;                  "basedpyright-langserver" "--stdio")))
;;;
;;;
;;;



;;; org clocking
;; make sure to define org-clocking-p before clocking, to avoid issues with
;; batch jobs / doom scripts
(defun org-clocking-p ()
  "Return t when clocking a task."
  (not (equal (org-clocking-buffer) nil)))


;;;  publishing

(setq org-link-file-path-type 'relative)
;;; fix org agenda direnv interactions


;; (defun my/disable-envrc-temporarily (orig-fun &rest args)
;;   "Disable `envrc-mode` temporarily when running Org Agenda."
;;   (let ((envrc-active envrc-mode)) ;; Check if envrc-mode is active
;;     (when envrc-active
;;       (envrc-mode -1)) ;; Disable envrc-mode
;;     (unwind-protect
;;         (apply orig-fun args)
;;       ;; Re-enable envrc-mode if it was active
;;       (when envrc-active
;;         (envrc-mode 1)))))

;; Apply this to Org Agenda and other relevant functions
;; (advice-add 'org-agenda :around #'my/disable-envrc-temporarily)
;; (advice-add 'org-agenda-list :around #'my/disable-envrc-temporarily)
;; (advice-add 'org-todo-list :around #'my/disable-envrc-temporarily)

;; (setq debug-on-message "Running direnv")



;;; w3m config

;; (require 'mime-w3m)
;; (w3m-search )
(map!
 :map w3m-mode-map
 :nv "J" #'w3m-previous-buffer

 ;;  only want yy in normal mode, so you can still visual select
 :n "yy"  #'w3m-print-this-url

 :n "f"  #'w3m-lnum-follow
 :n "F"  #'w3m-lnum-universal
 :nv "gh"  #'w3m-goto-home-page
 :nv "H"   #'w3m-view-previous-page
 :nv "L"   #'w3m-view-next-page
 :nv "r"   #'w3m-reload-this-page
 :nv "R"   #'w3m-reload-all-pages
 :nv "s"   #'w3m-search
 :nv "K" #'w3m-next-buffer
 :nv "X"   #'w3m-delete-buffer)

(map! :map w3m-mode-map
      "SPC T" #'w3m-copy-buffer)

;;; consult preferences

(setq case-fold-search t)

;;; use ob-python-extras
(use-package! ob-python-extras)

(after! (evil org ob-python-extras)

  (ob-python-extras-load-keybindings)
  (setq ob-python-extras/auto-format t)
  (setq ob-python-extras/allow-png-deletion t))


;;; use org-babel-alerts

(use-package! org-babel-alerts)

(after! 'org-babel-alerts

  (setq ob-babel-alerts/notification-command "notify-send -i emacs \"Org Block Finished\" \"Block in %b completed with result: %r\""
        "Command to run when a code block finishes.
Special format specifiers:
%b - buffer name
%f - buffer file name (or empty if no file)
%r - result content (or empty if no result)
%l - line number"))

;;; use gpt-babel

(use-package! gpt-babel)

(after! (evil org gpt-babel)

  (gpt-babel-load-keybindings)
  (gpt-babel/map-suggested-keyindings))

(setq gpt-babel/error-action nil)


;;; Useful for making packages:
;;;
(defun rename-module-functions ()
  (interactive)
  ;; First collect all defined functions
  (save-excursion
    (goto-char (point-min))
    (let (my-functions)
      ;; Collect all defined functions
      (while (re-search-forward "(defun \\([^/ ]+\\)" nil t)
        (push (match-string 1) my-functions))

      ;; Now for each function, rename its definition and calls
      (dolist (func my-functions)
        ;; Rename definition
        (goto-char (point-min))
        (while (re-search-forward (format "(defun %s\\b" (regexp-quote func)) nil t)
          (replace-match (format "(defun ob-python-extras/%s" func)))

        ;; Rename calls
        (goto-char (point-min))
        (while (re-search-forward (format "\\([^-a-zA-Z/]\\)%s\\b" (regexp-quote func)) nil t)
          (replace-match (format "\\1ob-python-extras/%s" func)))))))


;;;
;; (use-package! poly-org)

;;; org
;; Minimal UI
(after! org-modern
  (package-initialize)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  ;; (modus-themes-load-operandi)

  ;; Choose some fonts
  ;; (set-face-attribute 'default nil :family "Iosevka")
  ;; (set-face-attribute 'variable-pitch nil :family "Iosevka Aile")
  ;; (set-face-attribute 'org-modern-symbol nil :family "Iosevka")

  ;; Add frame borders and window dividers
  (modify-all-frames-parameters
   '((right-divider-width . 1)
     (internal-border-width . 1)))
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))

  (setq
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  ;; Ellipsis styling
  (setq org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

  (global-org-modern-mode))

;; (after! org
;;   (use-package! org-sliced-images
;;     :config
;;     (org-sliced-images-mode 1)))

(use-package! realgud
  :commands realgud:pdb)

;; (use-package! realgud-ipdb)




;;; Org-clipboard-download

(after! 'org
  (require 'org-download))



(setq org-download-image-dir "./plots/org-download/")
(setq org-download-method 'directory)

;; Drag-and-drop to `dired`
;; (add-hook 'dired-mode-hook 'org-download-enable)

(defun org-clipboard-has-image-p ()
  (interactive)
  ;; timeout necessary so xclip doesn't hang
  ;; this shell command is returning "" in emacs, though correct output if not
  ;; an image/png is the given error message -- compare to the terminal output of
  ;; the command.
  ;; to get the correct output in emacs the shell command needs
  ;; to be run async, which is more complicated to use here.
  ;; I haven't bothered to figure out why, checking for "" or the Error message
  ;; for stability
  (if (eq system-type 'darwin)
      (string-match-p "\\<PNG\\>" (shell-command-to-string "osascript -e 'clipboard info'"))
    (let ((output (shell-command-to-string "timeout 0.01 xclip -selection clipboard -t image/png -o")))
      (not (or (string-empty-p output)
               (string-match-p "Error: target image/png not available" output))))))

(defun org-clipboard-download-smart ()
  (interactive)
  (if (org-clipboard-has-image-p)
      (org-download-clipboard)
    (evil-paste-after 1))

  (map! :map org-mode-map
        :n "p" #'org-clipboard-download-smart))

;;; prog


;; this is much too general. I dont wnat this for python, fo rinstance.
;; (add-hook 'prog-mode-hook
;;           (lambda ()
;;             (add-hook 'before-save-hook 'eglot-format nil t)))


;;; narrowing
;;;


;; (use-package! org-src-context)
;; (setq  org-src-context-mode t)

;;; docview

(after! doc-view
  (map! :mode doc-view-mode
        :nm "]" #'doc-view-scroll-up-or-next-page
        :nm "[" #'doc-view-scroll-down-or-previous-page))


;; TODO[wvj9rr3eLT] A command that switches them
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-type 't)


(require 'ob-jupyter)


(require 'cl-lib)
;; (use-package! 'emacs-zmq)


(setq async-shell-command-buffer 'new-buffer)
;;; oneko

(use-package! oneko-macs)

;;; ob-python

;;; Bionic reading
;; From http://xahlee.info/emacs/emacs/elisp_bionic_reading.html

(defvar infu-bionic-reading-face nil "a face for `infu-bionic-reading-region'.")

(setq infu-bionic-reading-face 'bold)
;; try
;; 'bold
;; 'error
;; 'warning
;; 'highlight
;; or any value of M-x list-faces-display

(defun infu-bionic-reading-buffer ()
  "Bold the first few chars of every word in current buffer.
Version 2022-05-21"
  (interactive)
  (infu-bionic-reading-region (point-min) (point-max)))

(defun infu-bionic-reading-region (Begin End)
  "Bold the first few chars of every word in region.
Version 2022-05-21"
  (interactive "r")
  (let (xBounds xWordBegin xWordEnd  )
    (save-restriction
      (narrow-to-region Begin End)
      (goto-char (point-min))
      (while (forward-word)
        ;; bold the first half of the word to the left of cursor
        (setq xBounds (bounds-of-thing-at-point 'word))
        (setq xWordBegin (car xBounds))
        (setq xWordEnd (cdr xBounds))
        (setq xBoldEndPos (+ xWordBegin (1+ (/ (- xWordEnd xWordBegin) 2))))
        (put-text-property xWordBegin xBoldEndPos
                           'font-lock-face infu-bionic-reading-face)))))

;;; gptel


(setq gptel-display-buffer-action '(display-buffer-same-window))


;;;; quick lookup 
;; modified from the gptel wiki 

(defvar gptel-lookup--history nil)


(defun gptel-lookup (prompt)
  (interactive (list (read-string "Ask ChatGPT: " nil gptel-lookup--history)))
  (when (string= prompt "") (user-error "A prompt is required."))
  (gptel-request
      prompt
    :callback
    (lambda (response info)
      (cond
       ((not response)
        (message "gptel-lookup failed with message: %s"
                 (plist-get info :status)))

       ((and (consp response)
             (eq (car response) 'reasoning))
        (with-current-buffer (get-buffer-create "*gptel-reasoning*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert (format "Reasoning:\n%s" (cdr response))))
          (special-mode)
          (display-buffer (current-buffer)
                          `((display-buffer-in-side-window)
                            (side . bottom)
                            (window-height . ,#'fit-window-to-buffer)))))

       ((stringp response)
        (with-current-buffer (get-buffer-create "*gptel-lookup*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert response))
          (special-mode)
          (display-buffer (current-buffer)
                          `((display-buffer-in-side-window)
                            (side . bottom)
                            (window-height . ,#'fit-window-to-buffer)))))))))

(defun close-gptel-lookup-buffer ()
  "Close the gptel lookup buffer from anywhere."
  (interactive)
  (when-let ((buffer (get-buffer "*gptel-lookup*")))
    (when-let ((window (get-buffer-window buffer t)))
      (quit-window nil window))))

(defadvice! my-gptel-esc-handler (&rest _)
  :before #'keyboard-quit
  (when (get-buffer-window "*gptel-lookup*" t)
    (close-gptel-lookup-buffer)))

(set-popup-rule! "^\\*gptel-lookup\\*$"
  :side 'bottom
  :size 0.3
  :select nil
  :quit t)

(after! gptel
  (let ((eldritch-names '("Nyarlathotep" "Yog-Sothoth" "Azathoth" "Shub-Niggurath" "Cthulhu"
                          "Hastur" "Dagon" "Yig" "Tsathoggua" "Ithaqua" "Abhoth" "Atlach-Nacha"
                          "Cyäegha" "Ghatanothoa" "Mordiggian" "Nug" "Yeb" "Zoth-Ommog"))
        (mortal-titles '("Seeker of Forbidden Knowledge" "Disciple of Chaos" "Cultist"
                         "Paranormal Investigator" "Occult Scholar" "Keeper of Ancient Tomes"
                         "Dweller in Darkness" "Reader of Elder Signs" "Servant of the Old Ones"
                         "Wanderer in Dreams" "Delver of Secrets" "Mad Prophet"
                         "Observer of the Void" "Student of Non-Euclidean Geometry"
                         "Keeper of Forbidden Lore" "Witness to the Unknown")))
    (setf (alist-get 'org-mode gptel-prompt-prefix-alist)
          (format "* %s\n" (nth (random (length mortal-titles)) mortal-titles)))
    (setf (alist-get 'org-mode gptel-response-prefix-alist)
          (format "* %s\n" (nth (random (length eldritch-names)) eldritch-names))))

  (setq gptel-org-branching-context nil
        gptel-default-mode 'org-mode  
        gptel-max-tokens 8192
        gptel-rewrite-default-action 'ediff)

  (map! :nv "SPC o g g" 'gptel
        :nv "SPC o g q" 'gptel-lookup
        :nv "SPC o g a" 'gptel-add 
        :nv "SPC o g r" 'gptel-rewrite))

(use-package! gptel-quick)

;;; magit

;; make magit bearable in nixpkgs
;; (setq magit-refresh-verbose t)

(with-eval-after-load 'magit
  (defvar my/big-repos '("nixpkgs" )
    "List of strings identifying large repositories for magit optimization.")

  (defun my/is-big-repo ()
    (and (magit-toplevel)
         (seq-some (lambda (repo) (string-match-p repo (magit-toplevel)))
                   my/big-repos)))

  (defun my/maybe-modify-magit-sections ()
    (when (my/is-big-repo)
      (setq-local magit-status-sections-hook
                  (remove 'magit-insert-status-headers
                          (remove 'magit-insert-tags-header
                                  magit-status-sections-hook)))))

  (add-hook 'magit-status-mode-hook #'my/maybe-modify-magit-sections)

  (map! :nv "SPC g c a" 'magit-commit-amend))

;;; kill formatting

(defun disable-all-formatting ()
  "Disable all auto-formatting in nixpkgs repo."
  (interactive)
  (when (string-match-p "nixpkgs" (or (buffer-file-name) ""))
    (apheleia-mode -1)
    (setq-local before-save-hook nil)
    (setq-local eglot-format-on-save nil)
    (setq-local nix-format-on-save nil)))

;; (add-hook 'nix-mode-hook #'disable-all-formatting)

;;; avy

(map! :nv "s" 'evil-avy-goto-char-2
      :nv "S" (cmd! (let ((current-prefix-arg t))
                      (evil-avy-goto-char-2))))


;;; gptel chat logs

(defcustom gptel-chat-logs-directory "~/gptel-logs"
  "Directory to store GPTel conversation logs."
  :type 'directory
  :group 'gptel)

(defun gptel-save-and-commit-log ()
  "Save current GPTel buffer to ~/gptel-logs and commit with git."
  (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
         (filename (expand-file-name (concat (buffer-name) ".org")
                                     gptel-chat-logs-directory))
         (default-directory gptel-chat-logs-directory))
    (make-directory gptel-chat-logs-directory t)
    (write-region (point-min) (point-max) filename)
    (unless (file-exists-p ".git")
      (call-process "git" nil nil nil "init"))
    (call-process "git" nil nil nil "add" filename)
    (call-process "git" nil nil nil "commit" "-m" 
                  (concat "Chat log: " timestamp))))

(add-hook 'gptel-post-response-functions 
          (lambda (beg end) (when (not (= beg end)) (gptel-save-and-commit-log))))


;;; git
(defun get-github-branch-link ()
  "Get GitHub link for current file/directory and line on current branch."
  (interactive)
  (let* ((remote-url (string-trim (shell-command-to-string "git remote get-url origin")))
         (git-root (string-trim (shell-command-to-string "git rev-parse --show-toplevel")))
         (branch (string-trim (shell-command-to-string "git rev-parse --abbrev-ref HEAD")))
         (is-dired (eq major-mode 'dired-mode))
         (current-path (if is-dired
                           default-directory
                         buffer-file-name))
         (file-path (file-relative-name current-path git-root))
         (line-num (unless is-dired (line-number-at-pos)))
         (github-url (replace-regexp-in-string
                      "\\(.*\\)@\\(.*\\):\\(.*\\)\\.git"
                      "https://\\2/\\3"
                      remote-url)))
    (kill-new (if line-num
                  (format "%s/blob/%s/%s#L%d" github-url branch file-path line-num)
                (format "%s/tree/%s/%s" github-url branch file-path)))
    (message "Copied branch link to clipboard")))

(defun get-github-permalink ()
  "Get GitHub permalink for current file/directory."
  (interactive)
  (let* ((remote-url (string-trim (shell-command-to-string "git remote get-url origin")))
         (git-root (string-trim (shell-command-to-string "git rev-parse --show-toplevel")))
         (commit (string-trim (shell-command-to-string "git rev-parse HEAD")))
         (is-dired (eq major-mode 'dired-mode))
         (current-path (if is-dired
                           default-directory
                         buffer-file-name))
         (file-path (file-relative-name current-path git-root))
         (line-num (unless is-dired (line-number-at-pos)))
         (github-url (replace-regexp-in-string
                      "\\(.*\\)@\\(.*\\):\\(.*\\)\\.git"
                      "https://\\2/\\3"
                      remote-url)))
    (kill-new (if line-num
                  (format "%s/blob/%s/%s#L%d" github-url commit file-path line-num)
                (format "%s/tree/%s/%s" github-url commit file-path)))
    (message "Copied permalink to clipboard")))

;;; haskell

(require 'ob-haskell)
;; Enable org-babel for Haskell
(org-babel-do-load-languages
 'org-babel-load-languages
 '((haskell . t)))

;; (setq org-src-fontify-natively t)

;; Optional: Set default header arguments for Haskell blocks
;; (setq org-babel-default-header-args:haskell
;;       '((:results . "output drawer")
;;         (:exports . "both")))

;;; animations


;;; aider
(require 'aidermacs)
(setq aidermacs-backend 'vterm)

(global-set-key (kbd "C-a") 'aidermacs-transient-menu)
(setq aidermacs-vterm-multiline-newline-key "S-<return>")
(setq aidermacs-watch-files t)
(setq aidermacs-show-diff-after-change nil)
(setq aidermacs-auto-commits nil)



;;;  


(setq dired-listing-switches "-ahtl -v --group-directories-first")


;; (require 'ruff-format)
;; (add-hook 'python-mode-hook 'ruff-format-on-save-mode)

;;; Gathering Todos

(require
 'org-collect-code-todos)

(add-hook! 'doom-init-ui-hook #'org-collect-code-todos-mode)

(map! :map org-collect-code-todos-buffer-mode-map
      :n "C-c C-t" #'org-collect-code-todos-toggle-todo-state
      :n "C-c C-s" #'org-collect-code-todos-schedule
      :n "C-c C-d" #'org-collect-code-todos-deadline
      :n "C-c C-j" #'org-collect-code-todos-goto-org-todo)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((typescript . t)
   ))


;;;  agenda optimization


(add-hook 'org-agenda-mode-hook (lambda ()
                                  (when (eq major-mode 'org-agenda-mode)
                                    (envrc-mode -1))))

;;; Collect todo settings

(setq org-collect-code-todos-file "~/org/code-todos.org")


;; (with-eval-after-load 'org-agenda
;;   (add-to-list 'org-agenda-inhibit-startup envrc-mode))


(map! :mode gptel-mode "C-c m" #'gptel-menu)
(map! :mode gptel-mode "C-c t" #'gptel-tools)


;;; mcp
(add-hook 'after-init-hook
          #'mcp-hub-start-all-server)

(defun gptel-mcp-register-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (apply #'gptel-make-tool
                       tool))
            tools)))


(defun gptel-mcp-use-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (push (gptel-get-tool path)
                        gptel-tools)))
            tools)))



(defun gptel-mcp-close-use-tool ()
  (interactive)
  (let ((tools (mcp-hub-get-all-tool :asyncp t :categoryp t)))
    (mapcar #'(lambda (tool)
                (let ((path (list (plist-get tool :category)
                                  (plist-get tool :name))))
                  (setq gptel-tools
                        (cl-remove-if #'(lambda (tool)
                                          (equal path
                                                 (list (gptel-tool-category tool)
                                                       (gptel-tool-name tool))))
                                      gptel-tools))))
            tools)))

;;; remove spell completions

(remove-hook 'completion-at-point-functions #'ispell-completion-at-point)


(setq completion-at-point-functions
      (remove 'ispell-completion-at-point completion-at-point-functions))

(add-hook 'after-save-hook
          (lambda ()
            (when (eq major-mode 'latex-mode)
              (TeX-command-run-all nil))))


(use-package claude-code
  :config
  (map! "C-c c" claude-code-command-map))

(map! :map gptel-mode-map "C-c m" #'gptel-menu)


(map! :after vterm
      :map vterm-mode-map
      "C-c C-k" #'vterm-send-escape)

(setq claude-code-terminal-backend 'vterm)

;;; Emacs Server
(after! server
  (unless (server-running-p)
    (server-start)))
