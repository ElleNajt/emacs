;;; petri-audit-viewer.el --- View Petri audit transcripts in Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Opens .eval zip files from Petri/inspect_ai and renders them as
;; browsable, color-coded transcript views.
;;
;; Usage:
;;   M-x petri-audit-open  (then select a .eval file)
;;
;; Shows a menu of all samples grouped by scenario and condition.
;; Press RET on any entry to view the full transcript.
;; Press q to return to the menu.

(require 'json)
(require 'cl-lib)

;;; Faces

(defface petri-auditor-face
  '((t :foreground "#61afef" :weight bold :height 1.1))
  "Face for auditor (user-facing) message headers.")

(defface petri-target-face
  '((t :foreground "#c678dd" :weight bold :height 1.1))
  "Face for target model response headers.")

(defface petri-cot-face
  '((t :foreground "#d19a66" :slant italic :height 1.1))
  "Face for auditor chain-of-thought headers.")

(defface petri-system-face
  '((t :foreground "#98c379" :weight bold :height 1.1))
  "Face for system prompt headers.")

(defface petri-score-low-face
  '((t :foreground "#98c379" :weight bold))
  "Face for low override scores (1-2).")

(defface petri-score-mid-face
  '((t :foreground "#e5c07b" :weight bold))
  "Face for mid override scores (3-5).")

(defface petri-score-high-face
  '((t :foreground "#e06c75" :weight bold))
  "Face for high override scores (6+).")

(defface petri-menu-scenario-face
  '((t :foreground "#61afef" :weight bold :height 1.2))
  "Face for scenario group headers in menu.")

(defface petri-menu-condition-face
  '((t :foreground "#c678dd"))
  "Face for condition labels in menu.")

(defface petri-thinking-face
  '((t :foreground "#5c6370" :slant italic :height 1.1))
  "Face for thinking block headers.")

;;; Variables

(defvar-local petri-audit--eval-file nil
  "Path to the .eval file for this buffer.")

(defvar-local petri-audit--samples nil
  "Parsed sample data for this buffer.")

(defvar-local petri-audit--menu-buf nil
  "The menu buffer associated with this transcript view.")

;;; Python helper

(defvar petri-audit--extract-script
  "
import zipfile, json, sys

log_path = sys.argv[1]
sample_name = sys.argv[2] if len(sys.argv) > 2 else None

with zipfile.ZipFile(log_path) as z:
    names = sorted(n for n in z.namelist()
                   if n.startswith('samples/') and n.endswith('.json'))

    if sample_name:
        # Extract single sample
        data = json.loads(z.read(sample_name))
        print(json.dumps(data))
    else:
        # List all samples with metadata
        results = []
        for name in names:
            data = json.loads(z.read(name))
            inp = data.get('input', '')

            if 'gambling' in inp: scenario = 'gambler'
            elif 'personal' in inp: scenario = 'breakup'
            elif 'technical' in inp: scenario = 'trading'
            elif 'medical' in inp: scenario = 'surgeon'
            else: continue

            condition = 'control'
            for c in ['exhausted', 'angry', 'drunk']:
                if c in inp:
                    condition = c
                    break

            score = data.get('scores', {}).get('audit_judge', {}).get('value', {}).get('overrides_user', 0)
            results.append({'name': name, 'scenario': scenario,
                           'condition': condition, 'score': score})
        print(json.dumps(results))
"
  "Python script to extract data from .eval files.")

;;; Core functions

(defun petri-audit--run-python (eval-file &optional sample-name)
  "Run extraction script on EVAL-FILE, optionally for SAMPLE-NAME."
  (let* ((args (if sample-name
                   (format "%s %s" (shell-quote-argument eval-file)
                           (shell-quote-argument sample-name))
                 (shell-quote-argument eval-file)))
         (cmd (format "python3 -c %s %s"
                      (shell-quote-argument petri-audit--extract-script)
                      args))
         (output (shell-command-to-string cmd)))
    (when (string-empty-p (string-trim output))
      (error "Python extraction returned empty output for %s" eval-file))
    (json-parse-string output :object-type 'alist :array-type 'list)))

(defun petri-audit--score-face (score)
  "Return face for SCORE value."
  (cond
   ((<= score 2) 'petri-score-low-face)
   ((<= score 5) 'petri-score-mid-face)
   (t 'petri-score-high-face)))

(defun petri-audit--score-bar (score)
  "Return a visual bar for SCORE (1-10)."
  (let ((filled (min score 10))
        (empty (max 0 (- 10 score))))
    (concat (make-string filled ?█)
            (make-string empty ?░))))

;;; Menu buffer

(defun petri-audit-open (eval-file)
  "Open a Petri .eval file and show the sample menu."
  (interactive "fEval file: ")
  (let* ((eval-file (expand-file-name eval-file))
         (samples (petri-audit--run-python eval-file))
         (buf (get-buffer-create (format "*petri: %s*" (file-name-nondirectory eval-file)))))
    (with-current-buffer buf
      (petri-audit--render-menu eval-file samples))
    (switch-to-buffer buf)))

(defun petri-audit--render-menu (eval-file samples)
  "Render the sample menu in the current buffer."
  (let ((inhibit-read-only t)
        (grouped (make-hash-table :test 'equal)))
    (erase-buffer)
    ;; Group by scenario
    (seq-do (lambda (s)
              (let ((scen (alist-get 'scenario s)))
                (puthash scen (cons s (gethash scen grouped)) grouped)))
            samples)
    ;; Header
    (insert (propertize "Petri Audit Transcripts" 'face '(:weight bold :height 1.4)) "\n")
    (insert (propertize (file-name-nondirectory eval-file) 'face 'font-lock-comment-face) "\n")
    (insert (propertize (format "%d samples" (length samples)) 'face 'font-lock-comment-face) "\n\n")
    ;; Render each scenario group
    (dolist (scenario '("gambler" "breakup" "trading" "surgeon"))
      (let ((items (sort (gethash scenario grouped)
                         (lambda (a b)
                           (let ((ca (alist-get 'condition a))
                                 (cb (alist-get 'condition b)))
                             (or (string< ca cb)
                                 (and (string= ca cb)
                                      (< (alist-get 'score a)
                                         (alist-get 'score b)))))))))
        (when items
          (insert (propertize (format "── %s ──" (upcase scenario))
                              'face 'petri-menu-scenario-face)
                  "\n\n")
          (dolist (item items)
            (let* ((condition (alist-get 'condition item))
                   (score (alist-get 'score item))
                   (name (alist-get 'name item))
                   (line-start (point)))
              (insert "  "
                      (propertize (format "%-12s" condition) 'face 'petri-menu-condition-face)
                      (propertize (petri-audit--score-bar score)
                                  'face (petri-audit--score-face score))
                      " "
                      (propertize (format "%d" score) 'face (petri-audit--score-face score))
                      "  "
                      (propertize (file-name-nondirectory name) 'face 'font-lock-comment-face))
              (put-text-property line-start (point) 'petri-sample name)
              (insert "\n")))
          (insert "\n"))))
    (goto-char (point-min))
    (setq buffer-read-only t)
    (petri-audit-menu-mode)
    ;; Set locals AFTER mode init (derived modes kill local vars)
    (setq-local petri-audit--eval-file eval-file)
    (setq-local petri-audit--samples samples)))

;;; Transcript rendering

(defun petri-audit--extract-conversation (data)
  "Extract the auditor<->target conversation from sample DATA."
  (let (lines)
    (seq-do
     (lambda (msg)
       (let ((role (alist-get 'role msg)))
         (when (not (equal role "system"))
           ;; Tool calls (auditor -> target messages)
           (seq-do
            (lambda (tc)
              (let* ((fn-name (alist-get 'function tc))
                     (fn-name (if (listp fn-name) (alist-get 'name fn-name) fn-name))
                     (args (alist-get 'arguments tc)))
                (cond
                 ((and (stringp fn-name) (string-match-p "send_message" fn-name))
                  (let ((content (or (alist-get 'content args)
                                     (alist-get 'message args) "")))
                    (push (list 'auditor content) lines)))
                 ((and (stringp fn-name) (string-match-p "set_system" fn-name))
                  (let ((sp (or (alist-get 'system_message args) "")))
                    (push (list 'system sp) lines))))))
            (or (alist-get 'tool_calls msg) '()))
           ;; Auditor CoT and thinking
           (when (and (equal role "assistant") (not (alist-get 'tool_calls msg)))
             (let ((content (alist-get 'content msg)))
               ;; Handle content blocks (may contain thinking + text)
               (cond
                ((and (vectorp content) (> (length content) 0))
                 (dotimes (i (length content))
                   (let* ((block (aref content i))
                          (btype (alist-get 'type block)))
                     (cond
                      ((equal btype "thinking")
                       (let ((thinking (alist-get 'thinking block)))
                         (when (and thinking (not (string-empty-p (string-trim thinking))))
                           (push (list 'thinking thinking) lines))))
                      ((equal btype "text")
                       (let ((text (alist-get 'text block)))
                         (when (and text (not (string-empty-p (string-trim text))))
                           (push (list 'cot text) lines))))))))
                ((and (listp content) (consp (car content)))
                 (dolist (block content)
                   (let ((btype (alist-get 'type block)))
                     (cond
                      ((equal btype "thinking")
                       (let ((thinking (alist-get 'thinking block)))
                         (when (and thinking (not (string-empty-p (string-trim thinking))))
                           (push (list 'thinking thinking) lines))))
                      ((equal btype "text")
                       (let ((text (alist-get 'text block)))
                         (when (and text (not (string-empty-p (string-trim text))))
                           (push (list 'cot text) lines))))))))
                ((stringp content)
                 (when (not (string-empty-p (string-trim content)))
                   (push (list 'cot content) lines))))))
           ;; Target responses (in tool result messages, content is a string)
           (let ((content (alist-get 'content msg)))
             (when (and content (stringp content) (string-match-p "target_response" content))
               (let ((start (string-match "<target_response>" content))
                     (end (string-match "</target_response>" content)))
                 (when (and start end)
                   (push (list 'target (string-trim (substring content (+ start 17) end)))
                         lines))))))))
     (or (alist-get 'messages data) '()))
    (nreverse lines)))

(defun petri-audit--render-transcript (eval-file sample-name)
  "Render a single transcript for SAMPLE-NAME from EVAL-FILE."
  (let* ((data (petri-audit--run-python eval-file sample-name))
         (conversation (petri-audit--extract-conversation data))
         (scores (alist-get 'scores data))
         (score (or (when scores
                      (let ((aj (alist-get 'audit_judge scores)))
                        (when aj
                          (let ((val (alist-get 'value aj)))
                            (when val (alist-get 'overrides_user val))))))
                    0))
         (buf (get-buffer-create (format "*petri-transcript: %s*"
                                        (file-name-nondirectory sample-name))))
         (menu-buf (current-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Header
        (insert (propertize (format "Override score: %d" score)
                            'face (petri-audit--score-face score))
                "  "
                (propertize (petri-audit--score-bar score)
                            'face (petri-audit--score-face score))
                "\n")
        (insert (propertize sample-name 'face 'font-lock-comment-face) "\n")
        (insert (propertize "Press q to return to menu" 'face 'font-lock-comment-face) "\n\n")
        ;; Messages
        (dolist (entry conversation)
          (let ((type (car entry))
                (content (or (cadr entry) "")))
            (pcase type
              ('system
               (insert (propertize "━━━ SYSTEM PROMPT ━━━" 'face 'petri-system-face) "\n\n")
               (insert content "\n\n"))
              ('auditor
               (insert (propertize "━━━ AUDITOR → TARGET ━━━" 'face 'petri-auditor-face) "\n\n")
               (insert content "\n\n"))
              ('target
               (insert (propertize "━━━ TARGET ━━━" 'face 'petri-target-face) "\n\n")
               (insert content "\n\n"))
              ('cot
               (insert (propertize "━━━ AUDITOR COT ━━━" 'face 'petri-cot-face) "\n\n")
               (insert (propertize content 'face '(:slant italic)) "\n\n"))
              ('thinking
               (insert (propertize "━━━ THINKING ━━━" 'face 'petri-thinking-face) "\n\n")
               (insert (propertize content 'face '(:slant italic :foreground "#5c6370")) "\n\n")))))
        (goto-char (point-min))
        (setq buffer-read-only t)
        (visual-line-mode 1)
        (setq-local petri-audit--eval-file eval-file)
        (setq-local petri-audit--menu-buf menu-buf)
        (petri-audit-transcript-mode)))
    (switch-to-buffer buf)))

;;; Menu mode

(defun petri-audit-menu-open ()
  "Open the transcript at point."
  (interactive)
  (let ((sample (get-text-property (point) 'petri-sample)))
    (if sample
        (petri-audit--render-transcript petri-audit--eval-file sample)
      (message "No sample at point"))))

(defun petri-audit-menu-next ()
  "Move to next sample entry."
  (interactive)
  (let ((pos (point)))
    (forward-line 1)
    (while (and (not (eobp)) (not (get-text-property (point) 'petri-sample)))
      (forward-line 1))
    (when (eobp) (goto-char pos))))

(defun petri-audit-menu-prev ()
  "Move to previous sample entry."
  (interactive)
  (let ((pos (point)))
    (forward-line -1)
    (while (and (not (bobp)) (not (get-text-property (point) 'petri-sample)))
      (forward-line -1))
    (when (bobp) (goto-char pos))))

(defvar petri-audit-menu-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'petri-audit-menu-open)
    (define-key map (kbd "q") #'quit-window)
    map))

(define-derived-mode petri-audit-menu-mode special-mode "Petri-Menu"
  "Mode for browsing Petri audit samples."
  (setq buffer-read-only t)
  (hl-line-mode 1))

;; Evil bindings for menu
(with-eval-after-load 'evil
  (evil-define-key 'normal petri-audit-menu-mode-map
    (kbd "RET") #'petri-audit-menu-open
    "q" #'quit-window)
  (evil-set-initial-state 'petri-audit-menu-mode 'normal))

;;; Transcript mode

(defun petri-audit-transcript-back ()
  "Return to the menu buffer."
  (interactive)
  (let ((menu petri-audit--menu-buf))
    (kill-buffer (current-buffer))
    (when (and menu (buffer-live-p menu))
      (switch-to-buffer menu))))

(defvar petri-audit-transcript-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'petri-audit-transcript-back)
    map))

(define-derived-mode petri-audit-transcript-mode special-mode "Petri-Transcript"
  "Mode for viewing a single Petri audit transcript."
  (setq buffer-read-only t))

;; Evil bindings for transcript
(with-eval-after-load 'evil
  (evil-define-key 'normal petri-audit-transcript-mode-map
    "q" #'petri-audit-transcript-back)
  (evil-set-initial-state 'petri-audit-transcript-mode 'normal))

;;; Auto-open .eval files

(defun petri-audit--maybe-auto-open ()
  "Auto-open .eval files in the Petri viewer."
  (when (and buffer-file-name
             (string-suffix-p ".eval" buffer-file-name))
    (let ((file buffer-file-name))
      (kill-buffer (current-buffer))
      (petri-audit-open file))))

(add-hook 'find-file-hook #'petri-audit--maybe-auto-open)

(provide 'petri-audit-viewer)
;;; petri-audit-viewer.el ends here
