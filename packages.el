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
