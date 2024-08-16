
;;; Org file locations

(setq
 org-agenda-files (append
                   (directory-files-recursively "~/Notes" "\\.org$"))
 org-directory "~/Notes")

(load! (concat "personal_common" ))


;;;
()
