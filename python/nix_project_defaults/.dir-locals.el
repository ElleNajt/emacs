((python-mode
  (python-shell-interpreter . "nix-shell --pure --run python")))

((python-mode
  (eglot-server-programs . (((python-mode) "nix-shell" "--pure" "--run" "pyright-langserver --stdio")))))
