{
  description = "horth";

  inputs = {
    cabal-plan-nix.url = "github:t4ccer/cabal-plan-nix";
    nixpkgs.follows = "cabal-plan-nix/nixpkgs";
    flake-parts.follows = "cabal-plan-nix/flake-parts";
    pre-commit-hooks-nix.follows = "cabal-plan-nix/pre-commit-hooks-nix";
  };

  outputs = inputs @ {
    self,
    nixpkgs,
    ...
  }:
    inputs.flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.cabal-plan-nix.flakeModule
      ];
      systems = nixpkgs.lib.systems.flakeExposed;
      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: {
        pre-commit.settings = {
          hooks = {
            alejandra.enable = true;
          };
        };

        cabalPackages = {
          horth = {
            inherit pkgs;
            src = ./.;
            ghc = pkgs.haskell.compiler.ghc943;
            id = "horth-0.1.0.0-inplace-horth";
          };
        };

        devShells.default = let
          stackWithSystemGHC = pkgs.writeShellScriptBin "stack" ''
            ${pkgs.stack}/bin/stack --system-ghc --no-nix "$@"
          '';
        in
          pkgs.mkShell {
            nativeBuildInputs = [
              # MUST match stack snapshot
              pkgs.haskell.compiler.ghc943
              pkgs.cabal-install
              stackWithSystemGHC
              pkgs.haskell.packages.ghc925.fourmolu_0_9_0_0
              pkgs.haskell.packages.ghc943.haskell-language-server
              pkgs.zlib
              pkgs.yamlfix
              pkgs.alejandra
            ];
          };
        formatter = pkgs.alejandra;
        apps.plan2nix = inputs.cabal-plan-nix.apps.${system}.plan2nix;
      };
    };
}
