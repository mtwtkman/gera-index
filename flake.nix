{
  description = "gera-index development environment";
  inputs.nixpkgs.url = "nixpkgs/nixpkgs-unstable";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      nixpkgsFor = forAllSystems (system: import nixpkgs { inherit system; });
    in
    {
      defaultPackage = forAllSystems (system: self.packages.${system}.gera-index);
      checks = self.packages;
      devShell = forAllSystems (system: let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [];
          withHoogle = true;
          buildInputs = with haskellPackages; [
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
      });
  };
}
