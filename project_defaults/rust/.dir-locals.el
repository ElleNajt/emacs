((rust-mode
  (eglot-server-programs . (((rust-mode rust-ts-mode) "nix-shell" "-p" "rust-analyzer" "--run" "rust-analyzer")))))
