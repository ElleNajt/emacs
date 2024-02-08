;;; -*- lexical-binding: t; -*-

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

;; (start_time (current-time))
;; ( f (time-subtract (current-time) start-time) )
;;
(defun run-cell-and-advance () (interactive) (org-babel-execute-src-block) (org-babel-next-src-block) )
(map! (:mode org-mode
       :n "<S-return>" #'run-cell-and-advance
       :n "g SPC" #'org-babel-execute-buffer)
      (:mode org-agenda-mode
             "SPC m A" #'org-archive-subtree))

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

(advice-add
 'org-babel-python-evaluate-session
 :around
 #'ob-python--eval-python-session-with-exceptions)

(defun async--org-babel-execute-python (orig body &rest args)
  (let* ( (exec-file (make-temp-file "execution-code")))
    (with-temp-file exec-file (insert body))
    (let* ((body (format "\
exec_file = \"%s\"
try:
    with open(exec_file, 'r') as file:
        exec(compile(file.read(), '<org babel source block>', 'exec'))
except:
    import traceback
    print(traceback.format_exc())
finally:
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
 #'async--org-babel-execute-python)

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

(map! (:mode org-mode
       :n "C-c C-k" #'interrupt-org-babel-session))

;;; Writing a better interrupt function
(defun org-test ()
  (interactive)
  (let ((info (org-babel-get-src-block-info)))
    (if info
        (let ((session (cdr (assq :session info))))
          (if session
              (message "Session: %s" session)
            (message "No session found.")))
      (message "Not in a source block or no source block info found."))))

(defun org-src-block-end-header (&optional element)
  (let ((element (or element (org-element-at-point))))
    (save-excursion
      (goto-char (org-element-end element))
      (re-search-backward (rx (and bol "#+END_SRC")))
      (point))))

(defvar min-babel-exec-time-for-alert (* 60 3))
(defun add-babel-exec-time-alert-to-todo (src-block-element)
  )
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

(advice-add 'org-babel-execute-src-block :around #'timer-babel-execute-src-block-wrapper)

;;; Pandoc conversion
;;;
(defun run-ipynb-to-org-conversion-script ()
  (interactive)
  (when (dired-mode-p)
    (let ((current-dir (dired-current-directory))
          (script-path (concat doom-user-dir "bashscripts/convertnotebooks.sh" )))
      (shell-command (concat "cd " current-dir " && "script-path)))))
