{
  description = "Development environment for a project with vagrant, make, haskell and cabal";

  inputs = {
    # You can choose a specific channel or commit for reproducibility.
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        devShells= {

          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              vagrant             # Vagrant for virtual machine management
              gnumake                # GNU Make for build automation
              haskellPackages.ghc # The Glasgow Haskell Compiler
              haskellPackages.cabal-install # Cabal for building Haskell projects
            ];
          };

          aya = pkgs.mkShell {
            buildInputs = with pkgs; [
              rustup
              llvm
              pkg-config
              openssl
            ];
          };
       };
      });
}
