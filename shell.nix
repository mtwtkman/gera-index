{ pkgs ? import <nixpkgs> {} }:
with pkgs;
pkgs.mkShell {
  nativeBuildInputs = [
    haskell-language-server
    ghc
    cabal-install
    ormolu
    zlib
    nodejs
    nodePackages.typescript
    nodePackages.typescript-language-server
    chromium
  ];
  shellHook = ''
    alias fmt='ormolu -i $(git ls-files "*.hs")'
    alias t="cabal run test --"
    alias b="cabal build"
    alias run="cabal run gera-index"
    alias aggregate="node aggregator/dist/App.js"
    export PS1="\e[34;1m[dev]\W$ \e[0m"
    export PUPPETEER_EXECUTABLE_PATH=`which chromium`
  '';
}
