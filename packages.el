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

(package! comint-mime
  :recipe (:host github :repo "astoff/comint-mime"))

(package! texfrag)

(package! alert)


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


(package! python-black)
(package! python-isort)

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

(package! jcfk
  :recipe (:host github :repo "ElleNajt/org-sliced-images"))

;; (package! realgud)
;; (package! realgud-ipdb)

(package! org-src-context
  :recipe (:host github :repo "karthink/org-src-context"
           :files ("*.el")))

(package! emacs-zmq
  :recipe ( :host github :repo "nnicandro/emacs-zmq"))

;;(package! origami
;;  :recipe (:host github :repo "gregsexton/origami"))
;;
;;(package! ob-ipython
;;  :recipe (:host github :repo "gregsexton/ob-ipython"))

(package! evil-cleverparens)



(package! tidal
  :recipe (:local-repo "/Users/elle/code/Tidal"
           :files ("tidal.el")))

(package! org-inline-anim
  :recipe (
           :host github
           :repo
           "shg/org-inline-anim.el"
           ))

(package! aidermacs
  :recipe (
           :host github
           :repo "MatthewZMD/aidermacs"))

(package! org-babel-alert
  :recipe ( :host github :repo "ElleNajt/org-babel-alerts"))

;; (package! emacs-ruff-format
;;   :recipe (:host github :repo  "JoshHayes/emacs-ruff-format"))

(package! mcp-el
  :recipe (:host github
           :repo "lizqwerscott/mcp.el"))

(package! ob-typescript
  :recipe (:host github
           :repo "lurdan/ob-typescript"))

(package! org-collect-code-todos
  :recipe (:host github :repo "ElleNajt/org-collect-code-todos"))



(package! monet
  :recipe (:host github :repo "stevemolitor/monet"))

(package! eat)

(package! mistty)

(package! emacs-mcp
  :recipe (:host github :repo "ElleNajt/emacs-mcp"
           :files ("*.el" "example/*.el" "mcp-proxy.sh" "agents/*.md" "post-build-hook.el")))

;; Post-build hook that loads the actual hook implementation after download
(add-hook 'straight-use-package-post-build-functions
          (lambda (package)
            (when (string= package "emacs-mcp")
              (let ((hook-file (expand-file-name "post-build-hook.el" 
                                                 (straight--build-dir package))))
                (when (file-exists-p hook-file)
                  (load hook-file nil nil t)
                  (message "emacs-mcp: Post-build setup completed"))))))

(package! shell-maker)
(package! acp :recipe (:host github :repo "xenodium/acp.el"))
(package! agent-shell :recipe (:host github :repo "xenodium/agent-shell"))
(package! agent-shell-manager :recipe (:host github :repo "ElleNajt/agent-shell-manager"))

;; (package! minuet
;;   :recipe (:host github :repo "ElleNajt/minuet-ai.el"))

(package! obsidian
  :recipe (:host github :repo "licht1stein/obsidian.el"))

(package! spray
  :recipe (:host github :repo "emacsmirror/spray"))

(package! ement)
