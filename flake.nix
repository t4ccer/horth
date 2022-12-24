{
  description = "horth";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    ...
  }: let
    # I don't like default stack nix integration so here is my own
    stackWithSystemGHC = pkgs.writeShellScriptBin "stack" ''
      ${pkgs.stack}/bin/stack --system-ghc --no-nix "$@"
    '';
    pkgs = import nixpkgs {system = "x86_64-linux";};
  in {
    devShells.x86_64-linux.default = pkgs.mkShell {
      nativeBuildInputs = [
        # MUST match stack snapshot
        pkgs.haskell.compiler.ghc943
        pkgs.cabal-install
        stackWithSystemGHC
        pkgs.haskell.packages.ghc925.fourmolu_0_9_0_0
        pkgs.zlib
        pkgs.yamlfix
        pkgs.alejandra
      ];
    };
  };
}
