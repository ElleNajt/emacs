;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el
(package! predd
  :pin "695517712f0972cdb69b2765d13b45441ec4be57"
  :recipe (:host github :repo "skeeto/predd"))

(package! general
  :pin "a48768f85a655fe77b5f45c2880b420da1b1b9c3"
  :recipe (:host github :repo "noctuid/general.el"))

(package! ob-async :pin "9aac486073f5c356ada20e716571be33a350a982")
;; (package! pyvenv :pin "31ea715f2164dd611e7fc77b26390ef3ca93509b")
;; (package! python-black)

;; (package! paxedit :pin "09f3d5aeb108937a801e77ef413e29eaa4ecc4be")
;; (package! w3m :pin "ec18c21418bf7c1be159bd3cf7e79a370d4be1f3")
;; (package! combobulate
;;   :recipe (:host github :repo "mickeynp/combobulate"))

(package! outli
  :pin "009e74c1757143040a0427f477ae882107b14592"
  :recipe (:host github :repo "jdtsmith/outli"))

(package! cl-lib)
(package! dash
  :pin "d3a84021dbe48dba63b52ef7665651e0cf02e915"
  :recipe (:host github
           :repo "magnars/dash.el"))

(package! s :pin "dda84d38fffdaf0c9b12837b504b402af910d01d")
(package! hotfuzz :pin "ff72f544e03dd2afb358f28014b15529104c1d89")

(load! (concat "computers/" (string-trim (shell-command-to-string "hostname")) "-packages"))

;; (package! org-nix-shell
;;   :pin "f359d9e1053fadee86dd668f4789ae2e700d8e8a"
;;   :recipe (:host github
;;            :repo "AntonHakansson/org-nix-shell"))

;; (package! ts-fold
;;   :recipe (:host github :repo "ts-fold/ts-fold"))

;; (package! evil-textobj-tree-sitter
;;   :recipe (:host github
;;            :repo "meain/evil-textobj-tree-sitter"
;;            :files (:defaults "queries" "treesit-queries")))

(package! auctex :pin "f0c4b1dcc9e5987dce43b1e43f530351157ff577")
(package! company-auctex :pin "9400a2ec7459dde8cbf1a5d50dfee4e300ed7e18")

(package! exec-path-from-shell :pin "7552abf032a383ff761e7d90e6b5cbb4658a728a")

(package! org-download
  :pin "c8be2611786d1d8d666b7b4f73582de1093f25ac"
  :recipe (:host github
           :repo "abo-abo/org-download"))

(package! ob-python-extras
  :pin "faf7e568a03f8033fd20dc4cfd2ccaa750aae414"
  ;; :recipe (:local-repo "home/code/ob-python-extras"
  ;;          :files ("*.el" "bashscripts" "python")))

  :recipe (:host github
           :repo "ElleNajt/ob-python-extras"
           :branch "Development"
           :files ("*.el" "bashscripts" "python")))

(package! org-modern
  :pin "b4b5b1c864f1fdf240d1bbd7093529f5a75e8a06"
  :recipe (:host github
           :repo "minad/org-modern"))

;; (package! poly-org
;;   :recipe (:host github
;;            :repo "polymode/poly-org"))


;; (package! gptel
;;   :recipe (:host github
;;            :branch "fix-source-block-formatting"
;;            :repo "ElleNajt/gptel"))

;; (package! gptel
;;   :pin "63795dee75db49a04fd87842a1dcdef29c855f93"
;;   :recipe (:host github
;;            ;; :branch "curl_noproxy_variable"
;;            :repo "karthink/gptel"))

;; (package! realgud)
;; (package! realgud-ipdb)

;;(package! origami
;;  :recipe (:host github :repo "gregsexton/origami"))
;;
;;(package! ob-ipython
;;  :recipe (:host github :repo "gregsexton/ob-ipython"))

;; (package! emacs-ruff-format
;;   :recipe (:host github :repo  "JoshHayes/emacs-ruff-format"))

;; (package! mcp-el
;;   :recipe (:host github
;;            :repo "lizqwerscott/mcp.el"))

(package! org-collect-code-todos
  :pin "5754679d0c44eae484aab7a024c40906d755c83b"
  :recipe (:host github :repo "ElleNajt/org-collect-code-todos"))


(when (member (string-trim (shell-command-to-string "hostname")) '("aria" "fugue"))
  (package! shell-maker :pin "11f4a9913e7625f122625dd89d668ad5c93cf151")

  (package! acp
    :pin "661af51569acef7384a2801f07a582da5142a6d9"
    :recipe (:host github :repo "xenodium/acp.el"))
  (package! agent-shell
    :pin "89bd6e136a08e1527dd630e4573639c838fd7e22"
    :recipe (:host github :repo "xenodium/agent-shell"))

  (package! agent-shell-manager
    :pin "6d8c741999f30a755fd442cb73adde1fa506eeac"
    :recipe (:host github :repo "ElleNajt/agent-shell-manager"))
  (package! meta-agent-shell
    :pin "d1f4622b0f99105d7be2dd38a714fe7b9b5f49f5"
    :recipe (:host github :repo "ElleNajt/meta-agent-shell")))


