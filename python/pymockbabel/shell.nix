{ pkgs ? import <nixpkgs> { } }:

let
  pythonEnv = pkgs.python3.withPackages
    (ps: with ps; [ pandas numpy scikit-learn matplotlib seaborn httpx ]);
in pkgs.mkShell {
  buildInputs = [ pythonEnv pkgs.pyright ];
  shellHook = ''
    PYTHONPATH=${pythonEnv}/${pythonEnv.sitePackages}
  '';
  # Inspired by https://cyberchris.xyz/posts/nix-python-pyright/
}
