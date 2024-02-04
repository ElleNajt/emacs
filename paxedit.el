;;; -*- lexical-binding: t; -*-



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
