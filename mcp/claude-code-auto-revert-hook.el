;;; claude-code-auto-revert-hook.el --- Git merge or simple revert hook -*- lexical-binding: t; -*-
;; Version: 0.4.0
;; Package-Requires: ((emacs "30.0") (claude-code "0.2.0"))
;; Keywords: tools, ai

;;; Commentary:
;; Claude Code hook: auto-revert with three-way git merge.
;;
;; Pre-hook: auto-saves modified buffers, captures file state before edits.
;; Post-hook: three-way merges user buffer changes with Claude's disk changes.

;;; Code:

(require 'json)

(defvar claude-code--file-bases (make-hash-table :test 'equal)
  "Hash table storing original file contents before Claude edits.
Keys are file paths, values are the original contents.")


(defun claude-code-auto-revert-pre-tool-listener (message)
  "Pre-tool-use hook to save file state before Claude edits.
Also auto-saves any open buffers for the target file to prevent conflicts.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys."
  (when (eq (plist-get message :type) 'pre-tool-use)
    (condition-case err
        (let* ((json-data (plist-get message :json-data))
               (parsed-data (when (and json-data (stringp json-data))
                              (condition-case parse-err
                                  (json-read-from-string json-data)
                                (error
                                 (message "[Claude Pre-Hook] JSON parse error: %s" parse-err)
                                 nil))))
               (tool-name (when parsed-data (alist-get 'tool_name parsed-data)))
               (tool-input (when parsed-data (alist-get 'tool_input parsed-data))))

          (when (and tool-name (member tool-name '("Edit" "Write" "MultiEdit" "NotebookEdit" "Update")))
            (let ((file-path (or (alist-get 'file_path tool-input)
                                 (alist-get 'notebook_path tool-input))))


              (when (and file-path
                         (not (string-match-p "hook" file-path)))

                ;; Auto-save open buffers before Claude edits
                (let ((target-buffer (find-buffer-visiting file-path)))
                  (when (and target-buffer (buffer-modified-p target-buffer))
                    (with-current-buffer target-buffer
                      (save-buffer))))

                ;; Store the base content (current file state after auto-save)
                (let ((base-content (if (file-exists-p file-path)
                                        (with-temp-buffer
                                          (insert-file-contents file-path)
                                          (buffer-string))
                                      ""))) ; Empty base for new files
                  (puthash file-path base-content claude-code--file-bases))))))

      (error
       (message "[Claude Pre-Hook] Error: %s" err)))))

(defun claude-code-auto-revert-post-tool-listener (message)
  "Auto-revert hook with git-merge or simple revert.
MESSAGE is a plist with :type, :buffer-name, :json-data, and :args keys."
  (when (eq (plist-get message :type) 'post-tool-use)
    (condition-case err
        (let* ((json-object (json-read-from-string (plist-get message :json-data)))
               (tool-name (cdr (assoc 'tool_name json-object)))
               (params (cdr (assoc 'tool_input json-object))))

          (when (member tool-name '("Edit" "Write" "MultiEdit" "NotebookEdit"))
            (let ((file-path (or (cdr (assoc 'file_path params))
                                 (cdr (assoc 'notebook_path params)))))

              (when (and file-path
                         (not (string-match-p "hook" file-path)))

                (let ((target-buffer (find-buffer-visiting file-path)))
                  (when target-buffer
                    (with-current-buffer target-buffer
                      (if (buffer-modified-p)
                          (claude-code--auto-revert-git-merge file-path)
                        (revert-buffer t t t)))))))))
      (error
       (message "[Claude Revert] Error: %s" err)))))

(defun claude-code--auto-revert-git-merge (file-path)
  "Perform git merge between buffer changes and file changes.

  ARCHITECTURE:
  1. User is editing buffer (may have unsaved changes)
  2. Claude edits the file on disk
  3. This function merges both sets of changes

  THE PROBLEM:
  - User's buffer: has user's changes
  - File on disk: has Claude's changes
  - We don't have the original state before either made changes

  THE SOLUTION:
  - Use git merge-file in a clever way
  - Treat user's buffer as the 'base'
  - This makes git apply Claude's changes as a patch to user's buffer"

  (let* ((buffer-content (buffer-string))
         (base-content (gethash file-path claude-code--file-bases))
         (temp-base-file (make-temp-file "claude-base" nil ".tmp"))
         (temp-user-file (make-temp-file "claude-user" nil ".tmp"))
         (temp-claude-file (make-temp-file "claude-changes" nil ".tmp"))
         (temp-result-file (make-temp-file "claude-result" nil ".tmp")))


    ;; Write all three versions to temp files
    (with-temp-file temp-user-file
      (insert buffer-content))

    (with-temp-file temp-claude-file
      (insert-file-contents file-path))

    (if base-content
        (progn
          ;; We have the base! Do a proper three-way merge
          (with-temp-file temp-base-file
            (insert base-content))

          ;; Copy user's file as starting point
          (copy-file temp-user-file temp-result-file t)

          (let ((temp-files (list temp-base-file temp-user-file
                                  temp-claude-file temp-result-file)))
            (make-process
             :name "claude-merge"
             :command (list "git" "merge-file"
                            temp-result-file temp-base-file temp-claude-file)
             :sentinel
             (lambda (proc _event)
               (when (memq (process-status proc) '(exit signal))
                 (unwind-protect
                     (process-merge-result
                      temp-result-file file-path (process-exit-status proc))
                   ;; Cleanup temp files
                   (dolist (f temp-files)
                     (when (file-exists-p f)
                       (delete-file f)))
                   (remhash file-path claude-code--file-bases)))))))

      ;; No base content - fallback to simple revert
      (revert-buffer t t t)
      ;; Cleanup temp files
      (dolist (f (list temp-base-file temp-user-file
                       temp-claude-file temp-result-file))
        (when (file-exists-p f)
          (delete-file f)))
      (remhash file-path claude-code--file-bases))))

(defun process-merge-result (result-file file-path exit-code)
  "Process the merge result and update the buffer."
  ;; Create temp buffer with merge result
  (let ((temp-buffer (generate-new-buffer "*claude-merge-result*")))
    (unwind-protect
        (progn
          ;; Load merge result into temp buffer
          (with-current-buffer temp-buffer
            (insert-file-contents result-file))
          ;; Replace file buffer contents with temp buffer contents
          (with-current-buffer (find-buffer-visiting file-path)
            (set-visited-file-modtime)
            (erase-buffer)
            (insert-buffer-substring temp-buffer)
            ;; Write directly to skip save hooks (formatters, linters)
            (write-region nil nil file-path nil 'quiet)
            (set-visited-file-modtime)
            (set-buffer-modified-p nil)))
      (kill-buffer temp-buffer)))


  (when (/= exit-code 0)
    (message "[Claude Revert] Merge conflicts - exit code %d" exit-code)))

(provide 'claude-code-auto-revert-hook)

;;; claude-code-auto-revert-hook.el ends here
