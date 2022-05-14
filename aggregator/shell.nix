with import <nixpkgs> {};
mkShell {
  buildInputs = [
    nodejs
    nodePackages.typescript
    nodePackages.typescript-language-server
    chromium
  ];
  shellHook = ''
    export PUPPETEER_EXECUTABLE_PATH=`which chromium`
  '';
}
