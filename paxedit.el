;;; -*- lexical-binding: t; -*-

(defmacro define-move-and-insert
    (name &rest body)
  `(defun ,name (count &optional vcount skip-empty-lines)
     ;; Following interactive form taken from the source for `evil-insert'
     (interactive
      (list (prefix-numeric-value current-prefix-arg)
            (and (evil-visual-state-p)
                 (memq (evil-visual-type) '(line block))
                 (save-excursion
                   (let ((m (mark)))
                     ;; go to upper-left corner temporarily so
                     ;; `count-lines' yields accurate results
                     (evil-visual-rotate 'upper-left)
                     (prog1 (count-lines evil-visual-beginning evil-visual-end)
                       (set-mark m)))))
            (evil-visual-state-p)))
     (atomic-change-group
       ,@body
       (evil-insert count vcount skip-empty-lines))))

(defun get-char (&optional point)
  "Get the character at the given `point' (defaulting to the current point),
without properties"
  (let ((point (or point (point))))
    (buffer-substring-no-properties point (+ 1 point))))

(define-move-and-insert grfn/insert-at-sexp-end
                        (when (not (equal (get-char) "("))
                          (backward-up-list))
                        (forward-sexp)
                        (backward-char))

(define-move-and-insert grfn/insert-at-sexp-start
                        (backward-up-list)
                        (forward-char))

(define-move-and-insert grfn/insert-at-form-start
                        (backward-sexp)
                        (backward-char)
                        (insert " "))

(define-move-and-insert grfn/insert-at-form-end
                        (forward-sexp)
                        (insert " "))

(after! paxedit
  (nmap
    ">" (general-key-dispatch 'evil-shift-right
          "e" 'paxedit-transpose-forward
          ")" 'sp-forward-slurp-sexp
          "(" 'sp-backward-barf-sexp
          "I" 'grfn/insert-at-sexp-end
          ;; "a" 'grfn/insert-at-form-end
          ))

  (nmap
    "<" (general-key-dispatch 'evil-shift-left
          "e" 'paxedit-transpose-backward
          ")" 'sp-forward-barf-sexp
          "(" 'sp-backward-slurp-sexp
          "I" 'grfn/insert-at-sexp-start
          ;; "a" 'grfn/insert-at-form-start
          )))

(defun +grfn/paxedit-kill (&optional n)
  (interactive "p")
  (or (paxedit-comment-kill)
      (when (paxedit-symbol-cursor-within?)
        (paxedit-symbol-kill))
      (paxedit-implicit-sexp-kill n)
      (paxedit-sexp-kill n)
      (message paxedit-message-kill)))

(map!
 (:after
  paxedit
  (:map paxedit-mode-map
   :i ";"                          #'paxedit-insert-semicolon
   :i "("                          #'paxedit-open-round
   :i "["                          #'paxedit-open-bracket
   :i "{"                          #'paxedit-open-curly
   :n [remap evil-yank-line]       #'paxedit-copy
   :n [remap evil-delete-line]     #'+grfn/paxedit-kill
   :n "g o"                        #'paxedit-sexp-raise
   :n [remap evil-join-whitespace] #'paxedit-compress
   :n "g S"                        #'paxedit-format-1
   :n "g k"                        #'paxedit-backward-up
   :n "g j"                        #'paxedit-backward-end)))


(add-hook 'emacs-lisp-mode-hook #'paxedit-mode)
