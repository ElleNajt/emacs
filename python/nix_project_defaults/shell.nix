{ pkgs ? import <nixpkgs> { } }:

let
  pythonEnv = pkgs.python3.withPackages (ps:
    with ps; [
      pandas
      numpy
      scikit-learn
      matplotlib
      seaborn
      # httpx
    ]);
in pkgs.mkShell {
  buildInputs = [ pythonEnv pkgs.pyright ];
}

# NOTE: To get changes here to be reflected in eglot, you'll need to
# M-x envrc-reload, then reload eglot.
