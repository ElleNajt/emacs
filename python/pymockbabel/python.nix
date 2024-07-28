{ pkgs ? import <nixpkgs> {} }:

pkgs.python3.withPackages (p: with p; [
  pandas
  numpy
  scikit-learn
  matplotlib
  seaborn
]
)
