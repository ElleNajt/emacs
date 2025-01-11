
;;; Org file locations

(setq
 org-agenda-files (append
                   (directory-files-recursively "~/code/" "\\.org$")
                   (directory-files-recursively "~/org" "\\.org$"))

 org-directory "~/org")

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
     ("v" "/mnt/shared/videos"                       "videos")
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


;;;  setup eglot booster
;; experimental

;; (use-package eglot-booster
;;   :after eglot
;;   :config (eglot-booster-mode))


;;;  Owning files
(after! dired
  (defun dired-chown-elle ()
    "Change ownership of marked files to user 'elle' in dired."
    (interactive)
    (let ((files (dired-get-marked-files)))
      (dolist (file files)
        (shell-command
         (format "echo %s | sudo -S chown elle %s"
                 (read-passwd "Password: ")
                 (shell-quote-argument file))))
      (revert-buffer)))

  (map! :map dired-mode-map
        :n "C-c o" #'dired-chown-elle))


;;; email

(setq +mu4e-gmail-accounts '(("lnajt4@gmail.com" . "/lnajt4")))

(setq mu4e-index-cleanup nil
      mu4e-index-lazy-check t)

(setq mu4e-maildir (expand-file-name "~/Maildir"))
