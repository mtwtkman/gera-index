{ pkgs ? import <nixpkgs> {} }:
with pkgs;
pkgs.mkShell {
  nativeBuildInputs = [
    haskell-language-server
    ghc
    cabal-install
    ormolu
    zlib
  ];
  shellHook = ''
    alias fmt="ormolu -i **/*.hs"
    export PS1="\e[1;34m[dev]\W$ \e[0m"
  '';
}
