;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! predd
  :recipe (:host github :repo "skeeto/predd"))
(package! general
  :recipe (:host github :repo "noctuid/general.el"))
(package! ob-async)
(package! pyvenv)
(package! python-black)
(package! paxedit)
(package! w3m)
(package! oauth2)
;;; Added tree sitter
(package! tree-sitter)
(package! tree-sitter-langs)
(package! elpy)
(package! combobulate
  :recipe (:host github :repo "mickeynp/combobulate"))

(package! evil-motion-trainer
  :pin "32472acace6be6d99af1ab16cecaaea4344471ec"
  :recipe (:host github
           :repo "martinbaillie/evil-motion-trainer"))

(package! outli
  :recipe (:host github :repo "jdtsmith/outli")
  )

(package! cl-lib)
(package! dash
  :recipe (:host github
           :repo "magnars/dash.el"))
(package! s)
(package! hotfuzz)

(load! (concat "computers/" (string-trim (shell-command-to-string "hostname")) "-packages"))

(package! org-nix-shell
  :recipe (:host github
           :repo "AntonHakansson/org-nix-shell"))

;; (package! ts-fold
;;   :recipe (:host github :repo "ts-fold/ts-fold"))

(package! evil-textobj-tree-sitter
  :recipe (:host github
           :repo "meain/evil-textobj-tree-sitter"
           :files (:defaults "queries" "treesit-queries")))
