;;; configs/vterm.el -*- lexical-binding: t; -*-

;;; My vterm is ocassionaly weird -- I'm not sure what this does but maybe it fixes it
(defun +elle/vterm-setup ()
  (hide-mode-line-mode)
  (setq-local evil-collection-vterm-send-escape-to-vterm-p t))

(add-hook 'vterm-mode-hook #'+elle/vterm-setup)

(map! (:map vterm-mode-map
            "<C-escape>" #'evil-normal-state))
