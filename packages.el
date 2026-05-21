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
;; (package! combobulate
;;   :recipe (:host github :repo "mickeynp/combobulate"))

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

(package! org-download
  :recipe (:host github
           :repo "abo-abo/org-download"))


(package! gpt-babel
  :recipe (:host github
           :repo "ElleNajt/gpt-babel"
           :branch "main"
           :files ("*.el")))

(package! ob-python-extras
  ;; :recipe (:local-repo "home/code/ob-python-extras"
  ;;          :files ("*.el" "bashscripts" "python")))

  :recipe (:host github
           :repo "ElleNajt/ob-python-extras"
           :branch "Development"
           :files ("*.el" "bashscripts" "python")))

(package! oneko-macs
  :recipe (:host github
           :repo "ElleNajt/oneko-macs"))

(package! org-modern
  :recipe (:host github
           :repo "minad/org-modern"))

;; (package! poly-org
;;   :recipe (:host github
;;            :repo "polymode/poly-org"))



;; (package! gptel
;;   :recipe (:host github
;;            :branch "fix-source-block-formatting"
;;            :repo "ElleNajt/gptel"))


(package! gptel
  :recipe (:host github
           ;; :branch "curl_noproxy_variable"
           :repo "karthink/gptel"))

(package! gptel-quick
  :recipe (:host github 
           :repo "karthink/gptel-quick"))


;; (package! realgud)
;; (package! realgud-ipdb)


;;(package! origami
;;  :recipe (:host github :repo "gregsexton/origami"))
;;
;;(package! ob-ipython
;;  :recipe (:host github :repo "gregsexton/ob-ipython"))



(package! org-babel-alert
  :recipe ( :host github :repo "ElleNajt/org-babel-alerts"))

;; (package! emacs-ruff-format
;;   :recipe (:host github :repo  "JoshHayes/emacs-ruff-format"))

;; (package! mcp-el
;;   :recipe (:host github
;;            :repo "lizqwerscott/mcp.el"))


(package! org-collect-code-todos
  :recipe (:host github :repo "ElleNajt/org-collect-code-todos"))



(package! monet
  :recipe (:host github :repo "stevemolitor/monet"))

(package! eat)

(package! mistty)



(package! shell-maker)
(package! acp :recipe (:local-repo "~/code/acp.el"))
(package! agent-shell :recipe (:local-repo "~/code/agent-shell"))
(package! agent-shell-manager :recipe (:host github :repo "ElleNajt/agent-shell-manager"))
(package! agent-shell-to-go :recipe (:local-repo "~/code/agent-shell-to-go"))
(package! meta-agent-shell :recipe (:local-repo "~/code/meta-agent-shell"))

;; (package! minuet
;;   :recipe (:host github :repo "ElleNajt/minuet-ai.el"))

(package! obsidian
  :recipe (:host github :repo "licht1stein/obsidian.el"))

(package! spray
  :recipe (:host github :repo "emacsmirror/spray"))

