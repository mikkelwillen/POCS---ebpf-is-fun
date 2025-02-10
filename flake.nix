{
  description = "Development environment for fun eBPF and aya";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in {
        devShells= {
          shellHook = ''
              fish
            '';

          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              vagrant             # Vagrant for virtual machine management
              gnumake                # GNU Make for build automation
              haskellPackages.ghc # The Glasgow Haskell Compiler
              haskellPackages.cabal-install # Cabal for building Haskell projects
            ];
          };

          aya = pkgs.mkShell {
            shellHook = ''
              fish
            '';

            buildInputs = with pkgs; [
              rustup
              llvm
              bpftool
              pkg-config
              openssl
            ];
          };
       };
      });
}
