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
    export PS1="\e[34;1m[dev]\W$ \e[0m"
  '';
}
