;;; -*- lexical-binding: t; -*-

(load! "personal_packages")

;; Local development versions (override GitHub versions from packages.el)
(package! agent-shell :recipe (:local-repo "~/code/agent-shell"))
(package! agent-shell-to-go :recipe (:local-repo "~/code/agent-shell-to-go"))
(package! meta-agent-shell :recipe (:local-repo "~/code/meta-agent-shell"))
