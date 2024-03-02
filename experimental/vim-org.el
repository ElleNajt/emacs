;;; vim-org.el -*- lexical-binding: t; -*-



;;; Trying to write a function here that will force me to learn more efficient vim commands.
(defvar my/key-press-timing (make-hash-table :test 'equal))

(defun my/limit-key-repeats ()
  (let* ((key (this-command-keys))
         (current-time (float-time))
         (data (gethash key my/key-press-timing))
         (last-time (if data (car data) 0))
         (count (if data (cdr data) 0)))
    (if (and (eq last-command this-command) (< (- current-time last-time) .1))
        (progn
          (if (> count 2)
              (setq this-command 'ignore)
            (puthash key (cons current-time (1+ count)) my/key-press-timing)))
      (puthash key (cons current-time 1) my/key-press-timing))))

(add-hook 'evil-normal-state-entry-hook
          (lambda ()
            (add-hook 'pre-command-hook 'my/limit-key-repeats)))

(add-hook 'evil-normal-state-exit-hook
          (lambda ()
            (remove-hook 'pre-command-hook 'my/limit-key-repeats)))
