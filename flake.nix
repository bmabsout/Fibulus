{
  description = "A very basic flake";

  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
          extensions = (with pkgs.vscode-extensions; [
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
              (pkgs.haskell.packages.ghc912.ghcWithPackages (ps: with ps; 
                [ linear arithmoi ])
            )
	          pkgs.haskell.packages.ghc912.haskell-language-server
            vscodium-with-extensions
          ];
        };
      }
    );
}
