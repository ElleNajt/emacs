
;;; Org file locations

(setq
 org-agenda-files (append
                   (directory-files-recursively "~/Notes" "\\.org$"))
 org-directory "~/Notes")

(load! (concat "personal_common" ))


;;;
()

;;; Dirvish settings
;; taken from https://github.com/alexluigit/dirvish/blob/main/docs/CUSTOMIZING.org

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("D" "~/Downloads/"                "Downloads")
     ("d" "~/.doom.d/"                "doom config")
     ("w" "~/org/personal_webpage"        "personal_webpage")
     ("n" "~/Notes/"                       "notes")
     ))
  ;; :config
  ;; (dirvish-peek-mode) ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode) ; similar to `treemacs-follow-mode'
  ;; (setq dirvish-mode-line-format
  ;;       '(:left (sort symlink) :right (omit yank index)))
  ;; (setq dirvish-attributes
  ;;       '(all-the-icons file-time file-size collapse subtree-state vc-state git-msg))
  ;; (setq delete-by-moving-to-trash t)
  ;; (setq dired-listing-switches
  ;;       "-l --almost-all --human-readable --group-directories-first --no-group")
  )
