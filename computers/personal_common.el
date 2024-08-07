;;; computers/personal_common.el -*- lexical-binding: t; -*-


(setq ruff-command "ruff check")


;;; Local keybindings:
(map! :mode python-mode
      (:nv "g RET" #'run/python))
(map! (:nv "g SPC" #'run/generic))

;;; Default python project

(defun new-python-project (project-name)
  "Create a new Python project folder with a specified structure in the current dired directory."
  (interactive "sProject name: ")
  (let* ((default-directory (dired-current-directory))
         (project-dir (concat default-directory project-name "/"))
         (defaults-dir (concat doom-user-dir "python/nix_project_defaults/"))
         (shell-nix (concat defaults-dir "shell.nix"))
         (test-org (concat defaults-dir "scratch.org"))
         (.dir-locals (concat defaults-dir ".dir-locals.el"))
         (.envrc (concat defaults-dir ".envrc"))
         (current-datetime (format-time-string "%Y-%m-%d %H:%M:%S")))
    (make-directory project-dir)
    (copy-file shell-nix (concat project-dir "shell.nix"))
    (write-region (with-temp-buffer
                    (insert-file-contents test-org)
                    (replace-regexp-in-string "\\$CURRENT_DATETIME" current-datetime (buffer-string)))
                  nil
                  (concat project-dir "test.org"))
    (copy-file .dir-locals (concat project-dir ".dir-locals.el"))
    (copy-file .envrc (concat project-dir ".envrc"))

    ;; Copy any other files from the defaults directory
    (dolist (file (directory-files defaults-dir t "^[^.]+"))
      (unless (member (file-name-nondirectory file) '("shell.nix" "test.org" ".dir-locals.el" ".envrc"))
        (copy-file file project-dir)))

    ;; Run direnv allow before moving into the directory
    (let ((default-directory project-dir))
      (shell-command "direnv allow"))

    (dired project-dir)
    (message "Python project '%s' created successfully." project-name)))
