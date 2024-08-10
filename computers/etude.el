
;;; Org file locations

(setq
 org-agenda-files (append
                   (directory-files-recursively "/mnt/shared/Notes" "\\.org$")
                   (directory-files-recursively "/mnt/shared/OrgModeJupyter" "\\.org$"))
 org-directory "/mnt/shared/Notes")

(load! (concat "personal_common.el" ))
