;;; transcript-viewer.el --- View AI agent transcripts in Emacs -*- lexical-binding: t; -*-

(require 'json)

(defface transcript-user-face
  '((t :foreground "#61afef" :weight bold :height 1.1))
  "Face for USER role headers.")

(defface transcript-assistant-face
  '((t :foreground "#c678dd" :weight bold :height 1.1))
  "Face for ASSISTANT role headers.")

(defface transcript-tool-face
  '((t :foreground "#98c379" :weight bold :height 1.1))
  "Face for TOOL role headers.")

(defvar-local transcript-view--source-file nil
  "The source file for this transcript view.")

(defvar-local transcript-view--active nil
  "Whether transcript view is active in this buffer.")

(defun transcript-view--parse-buffer-or-file (content &optional file)
  "Parse transcript from CONTENT string. FILE used for json_repair fallback."
  (condition-case nil
      (let ((parsed (json-parse-string content :object-type 'alist :array-type 'list)))
        (if (and (listp parsed) (assq 'messages parsed))
            (alist-get 'messages parsed)
          parsed))
    (json-parse-error
     (if file
         (let ((repaired (shell-command-to-string
                          (format "uv run python3 -c \"import json, json_repair; r = json_repair.repair_json(open('%s').read(), return_objects=True); msgs = r.get('messages', r) if isinstance(r, dict) else r; print(json.dumps(msgs))\"" file))))
           (json-parse-string repaired :object-type 'alist :array-type 'list))
       (let ((repaired (shell-command-to-string
                        (format "uv run python3 -c \"import json, json_repair, sys; r = json_repair.repair_json(sys.stdin.read(), return_objects=True); msgs = r.get('messages', r) if isinstance(r, dict) else r; print(json.dumps(msgs))\" <<'JSONEOF'\n%s\nJSONEOF" content))))
         (json-parse-string repaired :object-type 'alist :array-type 'list))))))

(defvar transcript-view--db-path
  (expand-file-name "~/code/AlignmentResearch/AnthropicFellows/Projects/ControlMonitors/results/control_monitors_v2.db")
  "Path to the control monitors database.")

(defface transcript-stats-face
  '((t :foreground "#e5c07b" :weight bold))
  "Face for catch rate statistics.")

(defun transcript-view--attack-id-from-path (file)
  "Extract attack ID from FILE path. Expects .../attacks/ATTACK-ID/transcript.json."
  (when (and file (string-match "/attacks/\\([^/]+\\)/transcript\\.json\\'" file))
    (match-string 1 file)))

(defun transcript-view--catch-rates (attack-id)
  "Query catch rates for ATTACK-ID from the database. Returns formatted string or nil."
  (when (and attack-id (file-exists-p transcript-view--db-path))
    (let ((output (shell-command-to-string
                   (format "sqlite3 '%s' \"SELECT r.monitor_config_id, SUM(r.flagged), COUNT(*) FROM results r WHERE r.attack_id='%s' AND r.status <> 'ERROR' AND NOT EXISTS (SELECT 1 FROM attacks a WHERE a.id = r.attack_id AND a.is_benign = 1) GROUP BY r.monitor_config_id ORDER BY r.monitor_config_id\""
                           transcript-view--db-path attack-id))))
      (when (and output (not (string-empty-p (string-trim output))))
        (mapconcat
         (lambda (line)
           (when (string-match "\\([^|]+\\)|\\([0-9]+\\)|\\([0-9]+\\)" line)
             (format "  %s: %s/%s caught"
                     (match-string 1 line)
                     (match-string 2 line)
                     (match-string 3 line))))
         (split-string (string-trim output) "\n" t)
         "\n")))))

(defface transcript-caught-face
  '((t :foreground "#e06c75"))
  "Face for CAUGHT monitor excerpts.")

(defface transcript-missed-face
  '((t :foreground "#56b6c2"))
  "Face for MISSED monitor excerpts.")

(defun transcript-view--monitor-excerpts (attack-id)
  "Get one caught and one missed response per monitor for ATTACK-ID."
  (when (and attack-id (file-exists-p transcript-view--db-path))
    (let* ((monitors-output (string-trim
                             (shell-command-to-string
                              (format "sqlite3 '%s' \"SELECT DISTINCT monitor_config_id FROM results WHERE attack_id='%s' AND status <> 'ERROR' ORDER BY monitor_config_id\""
                                      transcript-view--db-path attack-id))))
           (monitors (when (not (string-empty-p monitors-output))
                       (split-string monitors-output "\n" t)))
           result)
      (dolist (monitor monitors)
        (dolist (spec '((1 "CAUGHT" transcript-caught-face)
                        (0 "MISSED" transcript-missed-face)))
          (let* ((flagged (nth 0 spec))
                 (label (nth 1 spec))
                 (face (nth 2 spec))
                 (output (string-trim
                          (shell-command-to-string
                           (format "sqlite3 -separator '|||' '%s' \"SELECT response FROM results WHERE attack_id='%s' AND monitor_config_id='%s' AND status <> 'ERROR' AND flagged=%d LIMIT 1\""
                                   transcript-view--db-path attack-id monitor flagged)))))
            (when (not (string-empty-p output))
              (push (list label face monitor output) result)))))
      (nreverse result))))

(defun transcript-view--extract-text (content)
  "Extract text from CONTENT which may be a string or list of content blocks."
  (cond
   ((stringp content) content)
   ((and (listp content) (consp (car content)))
    ;; List of content blocks: [{"type": "text", "text": "..."}, ...]
    (mapconcat
     (lambda (block)
       (or (alist-get 'text block) ""))
     (seq-filter (lambda (block) (equal (alist-get 'type block) "text")) content)
     "\n"))
   (t (format "%s" content))))

(defun transcript-view--render-messages (messages &optional attack-id)
  "Render MESSAGES into current buffer, replacing contents.
Optionally show catch rates and monitor excerpts for ATTACK-ID."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (dolist (msg messages)
      (let* ((role (upcase (alist-get 'role msg "")))
             (content (alist-get 'content msg ""))
             (face (pcase role
                     ("USER" 'transcript-user-face)
                     ("ASSISTANT" 'transcript-assistant-face)
                     ("TOOL" 'transcript-tool-face)
                     (_ 'default))))
        (insert (propertize (format "━━━ %s ━━━" role) 'face face) "\n\n")
        (insert (transcript-view--extract-text content))
        ;; Render tool_calls if present
        (let ((tool-calls (alist-get 'tool_calls msg)))
          (when tool-calls
            (dolist (tc tool-calls)
              (let* ((func (or (alist-get 'function tc) (alist-get 'name tc) "?"))
                     (args (alist-get 'arguments tc))
                     (cmd (when (listp args) (alist-get 'cmd args))))
                (insert "\n" (propertize (format "[Tool: %s]" func) 'face 'transcript-tool-face) "\n")
                (if cmd
                    (insert cmd "\n")
                  (insert (json-serialize args) "\n"))))))
        (insert "\n")))
    (let ((rates (transcript-view--catch-rates attack-id)))
      (when rates
        (insert (propertize "━━━ CATCH RATES ━━━" 'face 'transcript-stats-face) "\n\n")
        (insert rates)
        (insert "\n\n")))
    (let ((excerpts (transcript-view--monitor-excerpts attack-id)))
      (dolist (excerpt excerpts)
        (let ((label (nth 0 excerpt))
              (face (nth 1 excerpt))
              (monitor (nth 2 excerpt))
              (response (nth 3 excerpt)))
          (insert (propertize (format "━━━ %s (%s) ━━━" label monitor) 'face face) "\n\n")
          (insert response)
          (insert "\n\n"))))
    (goto-char (point-min))
    (visual-line-mode 1)
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)))

(defun transcript-view-toggle ()
  "Toggle between raw JSON and rendered transcript view in current buffer."
  (interactive)
  (if transcript-view--active
      ;; Switch back to raw JSON
      (let ((inhibit-read-only t)
            (file (or transcript-view--source-file (buffer-file-name))))
        (erase-buffer)
        (when file (insert-file-contents file))
        (json-mode)
        (setq buffer-read-only nil)
        (setq transcript-view--active nil)
        (set-buffer-modified-p nil)
        (message "Transcript view off"))
    ;; Switch to rendered view
    (let* ((file (or transcript-view--source-file (buffer-file-name)))
           (content (if file
                        (with-temp-buffer
                          (insert-file-contents file)
                          (buffer-string))
                      (buffer-string)))
           (messages (transcript-view--parse-buffer-or-file content file)))
      (setq transcript-view--source-file file)
      (let ((aid (transcript-view--attack-id-from-path file)))
        (transcript-view--render-messages messages aid)
        (setq transcript-view--active t)
        (message "Transcript view on")))))

(defun transcript-view-attack (attack-id)
  "View a transcript attack from the database by ATTACK-ID."
  (interactive "sAttack ID: ")
  (let* ((db-path transcript-view--db-path)
         (content (shell-command-to-string
                   (format "sqlite3 '%s' \"SELECT transcript_content FROM attacks WHERE id='%s'\"" db-path attack-id)))
         (messages (transcript-view--parse-buffer-or-file content))
         (buf (get-buffer-create (format "*transcript: %s*" attack-id))))
    (with-current-buffer buf
      (transcript-view--render-messages messages attack-id)
      (setq transcript-view--active t))
    (switch-to-buffer buf)))

(defun transcript-view--maybe-auto-render ()
  "Auto-render transcript view if the file is named transcript.json."
  (when (and buffer-file-name
             (string-match-p "/transcript\\.json\\'" buffer-file-name))
    (let* ((content (buffer-string))
           (messages (transcript-view--parse-buffer-or-file content buffer-file-name))
           (aid (transcript-view--attack-id-from-path buffer-file-name)))
      (setq transcript-view--source-file buffer-file-name)
      (transcript-view--render-messages messages aid)
      (setq transcript-view--active t))))

(add-hook 'find-file-hook #'transcript-view--maybe-auto-render)

(provide 'transcript-viewer)
