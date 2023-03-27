{
  description = "A very basic flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          extensions = (with pkgs.vscode-extensions; [
            bbenoist.Nix
            haskell.haskell
            justusadam.language-haskell
          ]);
          vscodium-with-extensions = pkgs.vscode-with-extensions.override {
            vscode = pkgs.vscodium;
            vscodeExtensions = extensions;
          };
      in {
        devShell = pkgs.mkShell {
          buildInputs = [
            (pkgs.ghc.withPackages (ps: with ps; 
              [ finite-field arithmoi iso-deriving linear ])
            )
	    pkgs.haskell-language-server
            vscodium-with-extensions
          ];
          Test=123;
        };
      }
    );
}
