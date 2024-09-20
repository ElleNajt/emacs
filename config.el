;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


;;; Requirements
(require 'general)
(require 'predd)
;;; Evil
(general-evil-setup t)
(remove-hook 'doom-first-input-hook #'evil-snipe-mode)

;;; Theme
(setq doom-theme 'doom-outrun-electric)

;; (setq doom-theme 'doom-shades-of-purple)
;; (setq doom-theme 'doom-feather-dark)
;;;; Evil motion trainer

;; (global-evil-motion-trainer-mode 1)
;; (setq evil-motion-trainer-threshold 6)
;; (setq evil-motion-trainer-super-annoying-mode t)

;; (add-emt-advice evil-next-line
;;                 '(evil-search-forward evil-jumper/backward evil-snipe-s)
;;                 next-line)
;; (add-emt-advice evil-next-visual-line
;;                 '(evil-search-forward evil-jumper/backward evil-snipe-s)
;;                 next-line)
;; (add-emt-advice evil-previous-line
;;                 '(evil-search-backward evil-snipe-S evil-jumper/backward evil-find-char-backward)
;;                 previous-line)
;; (add-emt-advice evil-previous-visual-line
;;                 '(evil-search-backward evil-snipe-S evil-jumper/backward evil-find-char-backward))
;; (add-emt-advice evil-forward-char
;;                 '(evil-search-forward evil-find-char evil-snipe-f evil-snipe-s))
;; (add-emt-advice evil-backward-char
;;                 '(evil-search-backward evil-find-char-backward evil-snipe-F evil-snipe-S))


;; (add-emt-advice evil-next-line
;;                 '(evil-search-forward evil-jumper/backward evil-snipe-s)
;;                 next-line)
;;; Programming Languages

(map!
 :n "] e" 'flycheck-next-error
 :n "[ e" 'flycheck-previous-error


 )
;;;; Elisp
(evil-define-operator fireplace-eval-elisp (beg end)
  (pp-eval-expression (read (buffer-substring beg end))))

(nmap :keymaps 'emacs-lisp-mode-map
  "c" (general-key-dispatch 'evil-change
        "p" (general-key-dispatch 'fireplace-eval-elisp
              "p" 'eval-sexp-at-point
              "c" 'eval-last-sexp
              "d" 'eval-defun)))

;;;;; Paxedit
(load! "vendored/paxedit")

;;; Rust

;;;; Clojure

;;;; Python
;; (elpy-enable)

(add-hook 'python-mode-hook #'flymake-mode)
(add-hook 'python-ts-mode-hook #'flymake-mode)
(after! apheleia
  (setf (alist-get 'python-mode apheleia-mode-alist) '(ruff-isort ruff))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) '(ruff-isort ruff)))

(require 'py-isort)
;; (add-hook 'before-save-hook 'py-isort-before-save)

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

(defun rust/run ()
  (interactive)
  (save-buffer)
  (vterm-run-and-return (concat (format  "cd %s & clear & cargo run" (file-name-directory buffer-file-name)))))


(defun rust/check ()
  (interactive)

  (save-buffer)
  (vterm-run-and-return (concat (format  "cd %s & clear & cargo check" (file-name-directory buffer-file-name)))))



(defun run/python ()
  (interactive)
  (vterm-run-and-return (concat "nix-shell -p 'python3.withPackages (p: [p.ipython p.matplotlib p.pandas p.seaborn])' --run 'python " buffer-file-name "'")))

(defun run/generic ()
  "Make the current buffer file executable and run it in vterm."
  (interactive)
  (let ((file-name buffer-file-name))
    (if (not file-name)
        (message "Buffer is not visiting a file")
      (progn
        (vterm-run-and-return (concat "chmod +x " file-name))
        (vterm-run-and-return file-name)))))

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
 (:map dired-mode-map                   ;; TODO Why does this work?
       (:leader "d t" #'vterm-cd-to-dired-dir-and-switch)))

;; (add-hook 'dired-mode-hook 'dired-hide-details-mode)

;;; Loading computer specific files
(load! (concat "computers/" (string-trim (shell-command-to-string "hostname"))))
;;; Org mode


(map! (:mode org-mode
       :n "SPC f i"  #'org-toggle-inline-images
       :n "SPC f I"  #'org-display-inline-images
       :n "g s"  #'org-edit-special
       :n "] c" 'evil-next-flyspell-error
       :n "[ c" 'evil-prev-flyspell-error
       ))

;;;; Org edit special


;;;; Movement

(defun my-org-forward-paragraph (orig-fun &rest args)
  "Advice function to move forward by paragraphs, skipping over images."
  (let ((orig-point (point)))
    (when (not (apply orig-fun args))
      (goto-char (point-max)))
    (while (and (org-in-regexp org-image-inline-re 1)
                (< (point) orig-point))
      (apply orig-fun args))))

(defun my-org-backward-paragraph (orig-fun &rest args)
  "Advice function to move backward by paragraphs, skipping over images."
  (let ((orig-point (point)))
    (when (not (apply orig-fun args))
      (goto-char (point-min)))
    (while (and (org-in-regexp org-image-inline-re 1)
                (> (point) orig-point))
      (apply orig-fun args))))

;; (advice-add 'org-forward-paragraph :around #'my-org-forward-paragraph)
;; (advice-add 'org-backward-paragraph :around #'my-org-backward-paragraph)


;;;; Deduplicate
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
;; TODO Would be great to also get this to remove TODOs if the corresponding DONE exists.

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
    #print(\"___________________________\")
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
;; (define-advice org-kill-note-or-show-branches
;;     (:around (orig &rest args) interrupt-org-babel-session)
;;   (if (org-element-type-p (org-element-at-point) 'src-block)
;;       (interrupt-org-babel-session)
;;     (apply orig args)))

;;;;; pymockbabel


(defun elle/wrap-org-babel-execute-python-mock-plt (orig body &rest args)
  (let* ( (exec-file (make-temp-file "execution-code"))
          (pymockbabel-script-location (concat doom-user-dir "/python/pymockbabel")))
    (with-temp-file exec-file (insert body))
    (let* ((body (format "\
exec_file = \"%s\"
pymockbabel_script_location = \"%s\"
import sys
sys.path.append(pymockbabel_script_location)
import pymockbabel
outputs_and_file_paths, output_types, list_writer = pymockbabel.setup(\"%s\")
try:
    with open(exec_file, 'r') as file:
        exec(compile(file.read(), '<org babel source block>', 'exec'))
except:
    import traceback
    print(traceback.format_exc())
finally:
    pymockbabel.display(outputs_and_file_paths, output_types, list_writer)
    try:
        os.remove(exec_file)
    except:
        pass" exec-file pymockbabel-script-location (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
           (result (apply orig body args)))
      result)))

(advice-add
 'org-babel-execute:python
 :around
 #'elle/wrap-org-babel-execute-python-mock-plt)


;;;;; fix table plots


(defun elle/wrap-org-babel-execute-python-mock-table (orig body &rest args)
  (let* ( (exec-file (make-temp-file "execution-code"))
          (pymockbabel-script-location (concat doom-user-dir "/python/pymockbabel")))
    (with-temp-file exec-file (insert body))
    (let* ((body (format "\
exec_file = \"%s\"
pymockbabel_script_location = \"%s\"
import sys
sys.path.append(pymockbabel_script_location)
import print_org_df
print_org_df.enable()
try:
    with open(exec_file, 'r') as file:
        exec(compile(file.read(), '<org babel source block>', 'exec'))
except:
    import traceback
    print(traceback.format_exc())
finally:
    try:
        os.remove(exec_file)
    except:
        pass" exec-file pymockbabel-script-location (file-name-sans-extension (file-name-nondirectory buffer-file-name))))
           (result (apply orig body args)))
      result)))

(advice-add
 'org-babel-execute:python
 :around
 #'elle/wrap-org-babel-execute-python-mock-table)

;; (advice-remove 'org-babel-execute:python nil)
;;;;;; Garbage collection

(defun find-org-file-references ()
  "Find all file names referenced within [[]] in the current org buffer and return them as a list."
  (let (file-references)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\[file:\\([^]]+\\)\\]\\]" nil t)
        (let ((file-name (match-string 1)))
          (push file-name file-references))))
    file-references))

(defun delete-unused-pngs-in-buffer (buffer)
  "Delete .png files in the /plots/ directory that are not referenced in the org file corresponding to BUFFER."
  (with-current-buffer buffer
    (when (and (eq major-mode 'org-mode) (buffer-file-name))
      (let* ((org-file (buffer-file-name))
             (org-file-name (file-name-sans-extension (file-name-nondirectory org-file)))
             (plots-dir (concat (file-name-directory org-file) "plots/" org-file-name))
             (referenced-files (find-org-file-references))
             (png-files (when (and (file-directory-p plots-dir) (file-exists-p plots-dir))
                          (directory-files plots-dir t "\\.png$"))))
        (when png-files
          (dolist (png-file png-files)
            (let ((relative-png-file (file-relative-name png-file (file-name-directory org-file))))
              (unless (member relative-png-file referenced-files)
                (delete-file png-file)
                (message "Deleted: %s" png-file)))))))))

(defun delete-unused-pngs-in-all-org-files ()
  "Delete unused .png files in all open org files."
  (interactive)
  (dolist (buffer (buffer-list))
    (delete-unused-pngs-in-buffer buffer)))

(run-at-time 300 300 'delete-unused-pngs-in-all-org-files)

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

(defun run-org-to-ipynb-conversion-script ()
  (interactive)
  (when (dired-mode-p)
    (let ((current-dir (dired-current-directory))
          (script-path (concat doom-user-dir "bashscripts/converttoipynb.sh" )))
      (compile (concat "cd " current-dir " && "script-path)))))


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
;; TODO This runs into an 'invalid search bound: wrong side of point" error, but works otherwise
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
      )

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
       :n "g SPC" #'org-babel-execute-buffer
       :n "C-c C-k" #'interrupt-org-babel-session)
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

;; (after! hl-todo
;;   (setq hl-todo-keyword-faces
;;         `(("TODO"  . (:foreground "red" :weight bold))
;;           ;; You can customize other keywords here
;;           ("FIXME" . (:foreground "orange" :weight bold))
;;           ("NOTE"  . (:foreground "green" :weight bold)))))

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


;;; Timestamps

;; Adding a space because otherwise it breaks org mode

(defun my/org-time-stamp-advice (orig-fun &rest args)
  "Ensure a space is inserted before the Org timestamp."
  (unless (or (bolp) (eq (char-before) ?\s))
    (insert " "))
  (apply orig-fun args))

(advice-add 'org-time-stamp :around #'my/org-time-stamp-advice)

;;; Get directory as text

(defun doom/concat-text-files ()
  "Concatenate all text files in the current directory, separated by filenames."
  (interactive)
  (let ((output-buffer (get-buffer-create "*Concatenated Text*")))
    (with-current-buffer output-buffer
      (read-only-mode 0)  ; Ensure we can modify the buffer
      (erase-buffer)      ; Clear previous contents
      (shell-command (concat doom-user-dir "bashscripts/directoryastext.sh" ) output-buffer)
      (read-only-mode 1)) ; Make the buffer read-only again
    (display-buffer output-buffer)))

;; Optionally, bind this function to a key in dired-mode
(map! :after dired
      :map dired-mode-map
      "C-c c" #'doom/concat-text-files
      :n "L" #'dired-do-symlink)

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
(use-package eglot
  :config
  ;; Ensure `nil` is in your PATH.
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  :hook
  (nix-mode . eglot-ensure))

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

;; (defun org-current-is-todo ()
;;   (string= "TODO" (org-get-todo-state)))
;; (org-agenda-custom-commands
;;  '(
;;    ("N" "Next Actions"
;;     ((agenda)
;;      (tags-todo "@work"
;;                 ((org-agenda-overriding-header "Work")
;;                  (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
;;      (tags-todo "@home"
;;                 ((org-agenda-overriding-header "Home")
;;                  (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
;;      ))
;;    ("h" "At home" tags-todo "@home"
;;     ((org-agenda-overriding-header "Home")
;;      (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
;;    ("w" "Work" tags-todo "@work"
;;     ((org-agenda-overriding-header "Work")
;;      (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))
;;    ("F" "First Action"
;;     ((tags-todo "@first"
;;                 ((org-agenda-overriding-header "First Action")
;;                  (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))))
;;    ("W" "Waiting" todo "WAITING")
;;    ))
;; (org-stuck-projects '("+LEVEL>=2+LEVEL<=3-@notstuck/-CANCELLED-DONE"
;;                       ("TODO" "WAITING")
;;                       nil
;;                       ""))


;;; Turning off tree sitter

(setq tree-sitter-mode nil)

;;; Latex
(after! latex
  (setq TeX-auto-save t
        TeX-parse-self t
        TeX-syntactic-comment t
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

(after! eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode)
                 "basedpyright-langserver" "--stdio")))

;;;

;;;
;;;

;;; alignment

(defun align-tables (start end)
  (interactive)
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (beginning-of-line)
      (when (org-at-table-p)
        (org-table-align)

        )
      (forward-line 1)
      )
    ))


(defun adjust-org-babel-results (orig-fun &rest args)
  (message "in advice")
  ;;  terrible hack to get the starting point of the src block
  ;;  TODO make this better
  ;; (let ((top-of-src-block nil)
  ;;       (in-table nil))
  ;;   (when (>= (length args) 2)
  ;;     (let ((second-arg (nth 1 args)))
  ;;       (let ((fifth-element (nth 5 second-arg)))
  ;;         (when (numberp fifth-element)
  ;;           (setq top-of-src-block fifth-element)
  ;;           (save-excursion
  ;;             (goto-char top-of-src-block)
  ;;             (let ((end-of-results-block
  ;;                    (save-excursion
  ;;                      (goto-char top-of-src-block)
  ;;                      (setq in-table nil)
  ;;                      (cl-loop initially (forward-line 1)
  ;;                               for line from 1
  ;;                               until (eobp)
  ;;                               for line-content = (buffer-substring-no-properties
  ;;                                                   (line-beginning-position)
  ;;                                                   (line-end-position))
  ;;                               do (progn
  ;;                                    ;; (message "Line %d: %s" line line-content)
  ;;                                    ;; (message "In table: %s" in-table)
  ;;                                    (cond
  ;;                                     ((and (not in-table) (string-match-p "^[ \t]*|" line-content))
  ;;                                      (setq in-table t)
  ;;                                      (message "aligning")
  ;;                                      (org-table-align))
  ;;                                     ((and in-table (not (string-match-p "^[ \t]*|" line-content)))
  ;;                                      (setq in-table nil)))
  ;;                                    (when (string-match-p "^[ \t]*:END:[ \t]*$" line-content)
  ;;                                      (cl-return (point))))
  ;;                               do (forward-line 1)
  ;;                               finally return nil))))
  ;;               (align-tables top-of-src-block end-of-results-block))))))))

  (org-table-map-tables 'org-table-align)
  ;; gonna use this because it seems more likely to get better in the future through optimizations to emacs
  ;; this is also slow

  (org-display-inline-images))

(advice-add 'org-babel-insert-result :after #'adjust-org-babel-results)
;; (setq debug-on-message "Code block evaluation complete\\.")

(setq debug-on-message nil)
;; does async have a hook
