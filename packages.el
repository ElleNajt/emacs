;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! predd
  :recipe (:host github :repo "skeeto/predd"))
(package! general
  :recipe (:host github :repo "noctuid/general.el"))
(package! ob-async)
(package! pyvenv)
;; (package! python-black)

(package! paxedit)
(package! w3m)
(package! oauth2)
(package! elpy)

;; (package! combobulate
;;   :recipe (:host github :repo "mickeynp/combobulate"))

;; (package! evil-motion-trainer
;;   :pin "32472acace6be6d99af1ab16cecaaea4344471ec"
;;   :recipe (:host github
;;            :repo "martinbaillie/evil-motion-trainer"))

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

;; (package! evil-textobj-tree-sitter
;;   :recipe (:host github
;;            :repo "meain/evil-textobj-tree-sitter"
;;            :files (:defaults "queries" "treesit-queries")))

(package! auctex)
(package! company-auctex)

(package! exec-path-from-shell)

;;; Loading computer specific files
(load! (concat "computers/" (string-trim (shell-command-to-string "hostname")) "-packages"))

(package! org-fragtog)

(package! alert)


(package! org-download
  :recipe (:host github
           :repo "abo-abo/org-download"))


(package! ob-python-extras
  :recipe (:host github
           :repo "ElleNajt/ob-python-extras"
           :branch "Development"
           :files ("*.el" "bashscripts" "python")))

(package! org-modern
  :recipe (:host github
           :repo "minad/org-modern"))

;; (package! poly-org
;;   :recipe (:host github
;;            :repo "polymode/poly-org"))


(package! python-black)
(package! python-isort)

;; (package! gptel
;;   :recipe (:host github
;;            :branch "fix-source-block-formatting"
;;            :repo "ElleNajt/gptel"))


(package! gptel
  :recipe (:host github
           :repo "karthink/gptel"))


(package! jcfk
  :recipe (:host github :repo "ElleNajt/org-sliced-images"))
