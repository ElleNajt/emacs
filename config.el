;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;;; Requirements
(require 'general)
(require 'predd)
;;; Evil
(general-evil-setup t)
;; (remove-hook 'doom-first-input-hook #'evil-snipe-mode)
;;;; Evil motion trainer

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
;;; Programming Languages
;;;; Elisp
(evil-define-operator fireplace-eval (beg end)
  (pp-eval-expression (read (buffer-substring beg end))))

(nmap :keymaps 'emacs-lisp-mode-map
  "c" (general-key-dispatch 'evil-change
        "p" (general-key-dispatch 'fireplace-eval
              "p" 'eval-sexp-at-point
              "c" 'eval-last-sexp
              "d" 'eval-defun)))
;;;;; Paxedit
(load! "vendored/paxedit")
;;;; Python
(use-package! python-black
  :demand t
  :after python)
(add-hook! 'python-mode-hook #'python-black-on-save-mode)
;; (map! :leader :desc "Blacken Buffer" "m b b" #'python-black-buffer)
;; (map! :leader :desc "Blacken Region" "m b r" #'python-black-region)
;; (map! :leader :desc "Blacken Statement" "m b s" #'python-black-statement)

(defun vterm-run-and-return (command file)
  (save-selected-window (save-mark-and-excursion
                          (vterm-other-window)
                          (vterm-send-string (concat command file))
                          (vterm-send-return)
                          )))

(defun run/python ()
  (interactive)
  (vterm-run-and-return "python " buffer-file-name))

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
 (:map dired-mode-map
       (:leader "d t" #'vterm-cd-to-dired-dir-and-switch)))


;;; Loading computer specific files
(load! (concat "computers/" (string-trim (shell-command-to-string "hostname"))))
;;; Org mode

;;;; General
(auto-save-visited-mode)
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
                                 "* TODO %?\n%i\n%a%U" )
                                ("n" "Inbox-Note" entry
                                 (file+headline +org-capture-todo-file  "Inbox Notes")
                                 "* %?\n%i%T" )

                                ("j" "Journal entry" entry
                                 (file+olp+datetree +org-capture-journal-file)
                                 "* %U %?\n%i" :prepend t)
                                ("J" "Journal entry with link" entry
                                 (file+olp+datetree +org-capture-journal-file)
                                 "* %U %?\n%i\n%a" :prepend t))
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
;; (require 'org)
;; Loading in org-evil functions with keybindings removed and setting my own
(load! "vendored/org-evil/org-evil-core")
(load! "vendored/org-evil/org-evil-commands")
(load! "vendored/org-evil/org-evil-motion")

;; https://discourse.doomemacs.org/t/common-config-anti-patterns/119
(add-hook! 'org-mode-hook 'org-evil-mode)


(undefine-key! evil-motion-state-map "[ s" "] s")

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

(org-evil--define-key 'motion 'org-evil-motion-mode
                      "[[" 'org-evil-motion-backward-block-begin
                      "]]" 'org-evil-motion-forward-block-begin
                      "gH" 'org-evil-motion-up-heading-top
                      "gh" 'org-evil-motion-up-heading)

(map! (:mode org-mode
       :n "<up>" 'org-evil-motion-backward-heading
       :n "<down>" 'org-evil-motion-forward-heading))
;;;; Org-Babel Python
;;;;; Source Block Functions

(defun org-babel-goto-src-block-results ()
  (interactive)
  (goto-char (org-babel-where-is-src-block-result))
  )

(defun org-src-block-end-header (&optional element)
  (let ((element (or element (org-element-at-point))))
    (save-excursion
      (goto-char (org-element-end element))
      (re-search-backward (rx (and bol "#+END_SRC")))
      (point))))

(defun grfn/+org-insert-item (orig direction)
  (interactive)
  (if (and (org-in-src-block-p)
           (equal direction 'below))
      (grfn/insert-new-src-block)
    (funcall orig direction)))
(advice-add #'+org--insert-item :around #'grfn/+org-insert-item)

(defun org-src-block-results-end (src-block)
  (save-excursion
    (goto-char (org-element-begin src-block))
    (when-let (results-loc (org-babel-where-is-src-block-result))
      (goto-char results-loc)
      (goto-char (org-element-end (org-element-at-point)))
      (skip-chars-backward " \t\n")
      (point))))

(defun grfn/insert-new-src-block ()
  (interactive)
  (let* ((current-src-block (org-element-at-point))
         (point-to-insert
          (or (org-src-block-results-end current-src-block)
              (save-excursion
                (goto-char (org-element-end current-src-block))
                (skip-chars-backward " \t\n")
                (point))))
         (src-block-head (save-excursion
                           (goto-char (org-element-property
                                       :begin current-src-block))
                           (let ((line (thing-at-point 'line t)))
                             (if (not (s-starts-with? "#+NAME:" (s-trim line)))
                                 line
                               (forward-line)
                               (thing-at-point 'line t))))))
    (goto-char point-to-insert)
    (insert "\n\n")
    (insert src-block-head)
    (let ((contents (point-marker)))
      (insert "\n#+end_src\n")
      (goto-char contents))))

(defun run-cell-and-advance () (interactive) (org-babel-execute-src-block) (org-babel-next-src-block) )

;;;;; Exception Handling
(defun ob-python--eval-python-session-with-exceptions
    (orig session body &rest args)
  (let* ((exc-file (make-temp-file "session-exception"))
         (exec-file (make-temp-file "execution-code")))
    (unwind-protect
        (progn
          (with-temp-file exec-file (insert body))
          (let* ((body (format "\
try:
    with open(\"%s\", 'r') as file:
        exec(file.read())
except:
    import traceback
    with open(\"%s\", \"w\") as f:
        f.write(traceback.format_exc())
    raise"
                               exec-file
                               exc-file))
                 (result (apply orig session body args))
                 (exc-string (with-temp-buffer
                               (insert-file-contents exc-file)
                               (buffer-string))))
            (when (not (string-empty-p exc-string))
              (org-babel-eval-error-notify nil exc-string))
            result))
      (progn (delete-file exc-file)
             (delete-file exec-file)))))

(defun elle/wrap-org-babel-execute-python (orig body &rest args)
  (let* ( (exec-file (make-temp-file "execution-code")))
    (with-temp-file exec-file (insert body))
    (let* ((body (format "\
exec_file = \"%s\"
import time
# since this can cause collisions if something else in the python script gets named datetime
from datetime import datetime as org_babel_wrapper_datetime
start = org_babel_wrapper_datetime.now()
try:
    with open(exec_file, 'r') as file:
        exec(compile(file.read(), '<org babel source block>', 'exec'))
except:
    import traceback
    print(traceback.format_exc())
finally:
    print(\"___________________________\")
    print(\"Cell Timer: \", str(org_babel_wrapper_datetime.now() - start), \"\\n\")
    import os
    try:
        os.remove(exec_file)
    except:
        pass" exec-file))
           (result (apply orig body args)))
      result)))

(advice-add
 'org-babel-execute:python
 :around
 #'elle/wrap-org-babel-execute-python)

(defun python-org-header ()
  (interactive)
  (let ((session-name (read-string "Name of session: ")))
    (end-of-line)
    (insert (format "\n\n* Default
 :PROPERTIES:
 :header-args: :results output :async t :session %s
 :END:" session-name))))

(defun org-babel-get-session ()
  (interactive)
  (let* ((src-info (org-babel-get-src-block-info))
         (headers (nth 2 src-info))
         (session (cdr (assoc :session headers))))
    session))


(defun interrupt-org-babel-session ()
  (interactive)
  (let* ((current-session (org-babel-get-session))
         (session-buffer (and current-session
                              (concat "*" current-session "*"))))
    (when session-buffer
      (let ((proc (get-buffer-process (get-buffer session-buffer))))
        (when proc
          (interrupt-process proc)
          (message "Interrupted session: %s" current-session))))))

;; C-c C-k alread bound to something in org mode, we add advice to the function that its
;; bound to to interrupt the process if the cursor is in a source block
(define-advice org-kill-note-or-show-branches
    (:around (orig &rest args) org-C-c-C-k-interrupt-org-babel-session)
  (if (org-element-type-p (org-element-at-point) 'src-block)
      (interrupt-org-babel-session)
    (apply orig args)))

;;;;; Draft of interrupt function that sets alerts
(defun org-test ()
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if info
        (let ((session (cdr (assq :session info))))
          (if session
              (message "Session: %s" session)
            (message "No session found.")))
      (message "Not in a source block or no source block info found."))))

(defvar min-babel-exec-time-for-alert (* 60 3))
(defun add-babel-exec-time-alert-to-todo (src-block-element))

(defun timer-babel-execute-src-block-wrapper (orig &optional arg info params)
  "Wrap `org-babel-execute-src-block' to measure and display execution time."
  (let* ((start-time (current-time))
         ;; Execute the block and capture the result.
         (result (funcall orig arg info params))
         (end-time (current-time))
         (execution-time (float-time (time-subtract end-time start-time)))
         (results-end (org-src-block-results-end (org-element-at-point)))
         (time-string (format "\ntime: %.3fs" execution-time)))
    (save-excursion
      ;; Find and remove the existing "time :" line, if present.
      (goto-char results-end)
      (let ((next-heading (save-excursion (outline-next-heading) (point))))
        (while (re-search-forward
                (rx (and bol "time: "
                         (zero-or-more any)
                         eol
                         "\n"))
                next-heading
                t)
          (replace-match "" nil nil)))

      ;; Insert the new execution time.
      (goto-char results-end)
      (end-of-line)
      (insert time-string))
    (when (> execution-time min-babel-exec-time-for-alert)
      (min-babel-exec-time-alert-to-todo
       (org-element-at-point)))
    result))

;; (advice-add 'org-babel-execute-src-block :around #'timer-babel-execute-src-block-wrapper)

;;;;; Pandoc conversion script

(defun run-ipynb-to-org-conversion-script ()
  (interactive)
  (when (dired-mode-p)
    (let ((current-dir (dired-current-directory))
          (script-path (concat doom-user-dir "bashscripts/convertnotebooks.sh" )))
      (compile (concat "cd " current-dir " && "script-path)))))

(defun run-ipynb-to-org-conversion-script-recursively ()
  (interactive)
  (when (dired-mode-p)
    (let ((current-dir (dired-current-directory))
          (script-path (concat doom-user-dir "bashscripts/convertnotebooks.sh" )))
      (compile (concat "cd " current-dir " && "script-path " -r")))))


;;;;; For python editing in org files
;; (setq-default tab-width 2) -- TODO the version of this that actually works
;;;; Org-babel Nix
(add-hook 'org-mode  'org-nix-shell-mode)
;;;; Computer specific after org
(load! (concat "computers/" (string-trim (shell-command-to-string "hostname")) "-after"))
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
;;; Keybindings
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
    ["f"]
    (list "b" "Reset HEAD~"    #'magit-reset-head-back))
  (transient-append-suffix
    #'magit-reset
    ["f"]
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
  :nv "f" 'outline-forward-same-level
  :nv "p" 'outline-backward-same-level
  :nv "j" 'outline-next-heading
  :nv "k" 'outline-previous-heading
  :nv "n" 'outli-toggle-narrow-to-subtree  ; replacing evil-scroll-start-column
  ;; Leaving z N bound to doom/widen-indirectly-narrowed-buffer
  :nv "u" 'outline-up-heading
  ;; make condition using outli-on-heading-p
  :nv "," 'outline-promote
  :nv "." 'outline-demote
  :nv "<up>" 'outline-move-subtree-up
  :nv "<down>" 'outline-move-subtree-down
  :nv "i" 'outli-insert-heading-respect-content
  ;; :nv "1" '(outline-hide-sublevels 1)
  ;; :nv "2" '(outline-hide-sublevels 2)
  ;; :nv "3" '(outline-hide-sublevels 3)
  ;; :nv "4" '(outline-hide-sublevels 4)
  ;; :nv "5" '(outline-hide-sublevels 5)
  ))

;;;; Org python
(map! (:mode org-mode
       :n "<S-return>" #'run-cell-and-advance
       :n "g SPC" #'org-babel-execute-buffer)
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
  :n   "g SPC" #'eval-buffer ))

;;; Tree sitter
;;  Links to code downloaded from git
;; (setq combobulate-source-code-path "~/Documents/GitHub/combobulate")
;; (setq tsfold-source-code-path "~/Documents/GitHub/ts-fold")

;; (load! "../vendored/combobulate-config")
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
;;;; Evil Object Tree sitter
;; Just taking these from the evil-textobject-tree-sitter git
;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`

(define-key evil-outer-text-objects-map "f"
            (evil-textobj-tree-sitter-get-textobj "function.outer"))
;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
(define-key evil-inner-text-objects-map "f"
            (evil-textobj-tree-sitter-get-textobj "function.inner"))
;; You can also bind multiple items and we will match the first one we can find

(define-key evil-outer-text-objects-map "r"
            (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

(define-key evil-inner-text-objects-map "r"
            (evil-textobj-tree-sitter-get-textobj ("conditional.inner" "loop.inner")))


(define-key evil-inner-text-objects-map "a"
            (evil-textobj-tree-sitter-get-textobj ( "assignment.inner")))


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
(custom-set-faces!
  ;; `(hl-line :background "#000000")
  ;; `(default :background "#200000")
  ;; `(default :background "#200000")
  )
