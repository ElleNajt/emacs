;;; -*- lexical-binding: t; -*-

;; (require 'dash)
;; (require 'org-evil)
(require 'org)

(defun org-babel-goto-src-block-results ()
  (interactive)
  (goto-char (org-babel-where-is-src-block-result))
  )

;; ;;; This loads in a modified version of org evil
;; with the bindings commented out and which is prevented from byte compilation
;;
;;

(load! "org-evil/org-evil-commands")
(load! "org-evil/org-evil-motion")

;; https://discourse.doomemacs.org/t/common-config-anti-patterns/119
(add-hook! 'org-mode-hook 'org-evil-mode)


(undefine-key! evil-motion-state-map "[ s" "] s")

(map! (:mode org-mode
       :n "] r" #'org-babel-goto-src-block-results
       "[ s" 'org-evil-block-beginning-of-block
       "] s" 'org-evil-block-end-of-block))

(org-evil--define-key 'motion 'org-evil-block-mode
                      "[ s" 'org-evil-block-beginning-of-block
                      "] s" 'org-evil-block-end-of-block)

;; Have to loop through as it looks like the text objects
;; don't configure correctly when binding multiple states
;; at once.

(dolist (mode '(operator visual))
  (org-evil--define-key mode 'org-evil-block-mode
                        "ib" 'org-evil-block-inner-block
                        "ab" 'org-evil-block-a-block))


(org-evil--define-key 'motion 'org-evil-motion-mode
                      "[[" 'org-evil-motion-backward-block-begin
                      "]]" 'org-evil-motion-forward-block-begin
                      "gH" 'org-evil-motion-up-heading-top
                      "gh" 'org-evil-motion-up-heading)


;; ;; (undefine-key! evil-motion-state-map "<up>" "<down>")
;; ;; (undefine-key! global-map "<up>" "<down>")
;; (org-evil--define-key 'motion 'org-mode
;;                       "g [" 'org-evil-motion-backward-heading
;;                       "g ]" 'org-evil-motion-forward-heading)

(map! (:mode org-mode
       :n "<up>" 'org-evil-motion-backward-heading
       :n "<down>" 'org-evil-motion-forward-heading))
