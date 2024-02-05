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
              (org-element-end current-src-block)))
         (src-block-head (save-excursion
                           (goto-char (org-element-property
                                       :begin current-src-block))
                           (let ((line (thing-at-point 'line t)))
                             (if (not (s-starts-with? "#+NAME:" (s-trim line)))
                                 line
                               (forward-line)
                               (thing-at-point 'line t))))))
    (goto-char point-to-insert)
    (insert "\n")
    (insert src-block-head)
    (let ((contents (point-marker)))
      (insert "\n#+END_SRC\n")
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
  (let* ((exc-file (make-temp-file "session-exception")))
    (unwind-protect
        (let* ((body (format "\
try:
%s
except:
    import traceback
    with open(\"%s\", \"w\") as f:
        f.write(traceback.format_exc())
    raise"
                             (org-babel-python--shift-right body 4)
                             exc-file))
               (result (apply orig session body args))
               (exc-string (with-temp-buffer
                             (insert-file-contents exc-file)
                             (buffer-string))))
          (when (not (string-empty-p exc-string))
            (org-babel-eval-error-notify nil exc-string))
          result)
      (delete-file exc-file))))

(advice-add
 'org-babel-python-evaluate-session
 :around
 #'ob-python--eval-python-session-with-exceptions)

(defun async--org-babel-execute-python (orig body &rest args)
  (let* ((body (format "\
try:
%s
except:
    import traceback
    print(traceback.format_exc()) "
                       (org-babel-python--shift-right body 4)
                       ))
         (result (apply orig body args)))
    result))

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
