;;; indedit.el -*- lexical-binding: t; -*-

(defun get-char (&optional point)
  "Get the character at the given `point' (defaulting to the current point),
without properties"
  (let ((point (or point (point))))
    (buffer-substring-no-properties point (+ 1 point))))



;;;  combobulate-navigate-up
;;; combobulate-navigate-down
;;; combobulate-drag-up


(define-move-and-insert indent/insert-after-sexp-end
                        (when (not (equal (get-char) ")"))
                          (backward-up-list))
                        (forward-sexp))

(define-move-and-insert indent/insert-at-sexp-end
                        (when (not (equal (get-char) "("))
                          (backward-up-list))
                        (forward-sexp)
                        (backward-char))

(define-move-and-insert indent/insert-before-sexp-start
                        (backward-up-list))

(define-move-and-insert indent/insert-at-sexp-start
                        (backward-up-list)
                        (forward-char))

(define-move-and-insert indent/insert-at-form-start
                        (backward-sexp)
                        (backward-char)
                        (insert " "))

(define-move-and-insert indent/insert-at-form-end
                        (forward-sexp)
                        (insert " "))

(after! indedit
  (nmap
    ">" (general-key-dispatch 'evil-shift-right
          "e" 'indedit-transpose-forward
          ")" 'sp-forward-slurp-sexp
          "(" 'sp-backward-barf-sexp
          "I" 'indent/insert-at-sexp-end
          "A" 'indent/insert-after-sexp-end
          ;; "a" 'indent/insert-at-form-end
          ))

  (nmap
    "<" (general-key-dispatch 'evil-shift-left
          "e" 'indedit-transpose-backward
          ")" 'sp-forward-barf-sexp
          "(" 'sp-backward-slurp-sexp
          "I" 'indent/insert-at-sexp-start
          "A" 'indent/insert-before-sexp-start
          ;; "a" 'indent/insert-at-form-start
          )))

(defun +indent/indedit-kill (&optional n)
  (interactive "p")
  (or (indedit-comment-kill)
      (when (indedit-symbol-cursor-within?)
        (indedit-symbol-kill))
      (indedit-implicit-sexp-kill n)
      (indedit-sexp-kill n)
      (message indedit-message-kill)))

(map!
 (:after
  indedit
  (:map indedit-mode-map
   :i ";"                          #'indedit-insert-semicolon
   :i "("                          #'indedit-open-round
   :i "["                          #'indedit-open-bracket
   :i "{"                          #'indedit-open-curly
   :n [remap evil-yank-line]       #'indedit-copy
   :n [remap evil-delete-line]     #'+indent/indedit-kill
   :n "g o"                        #'indedit-sexp-raise
   :n [remap evil-join-whitespace] #'indedit-compress
   :n "g S"                        #'indedit-format-1
   :n "g k"                        #'indedit-backward-up
   :n "g j"                        #'indedit-backward-end)))


(add-hook 'emacs-python-mode-hook #'indedit-mode)
