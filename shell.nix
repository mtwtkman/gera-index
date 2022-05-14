{ pkgs ? import <nixpkgs> {} }:
with pkgs;
pkgs.mkShell {
  nativeBuildInputs = [
    haskell-language-server
    ghc
    cabal-install
    ormolu
    zlib
    jq
  ];
  shellHook = ''
    alias fmt='ormolu -i $(git ls-files "*.hs")'
    alias t="cabal run test --"
    alias b="cabal build"
    alias run="cabal run gera-index"
    export PS1="\e[34;1m[dev]\W$ \e[0m"
  '';
}
