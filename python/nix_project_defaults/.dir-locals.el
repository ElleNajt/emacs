((python-mode . (

                 ;; (python-shell-interpreter . "nix-shell --pure --run python")
                 ;; (eglot-server-programs . (((python-mode) "nix-shell" "--pure" "--run" "basedpyright-langserver --stdio")))
                 (eglot-workspace-configuration . (:python (:pythonpath "nix-shell --pure --run python"
                                                            :analysis (:extrapaths ""))))
                 (lsp-basedpyright-extra-paths . [""]))))
