{
  description = "horth";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.nixpkgs-stable.follows = "nixpkgs";
    };
    dream2nix = {
      url = "github:nix-community/dream2nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.pre-commit-hooks.follows = "pre-commit-hooks-nix";
      inputs.flake-parts.follows = "flake-parts";
      inputs.all-cabal-json.follows = "all-cabal-json";
    };
    all-cabal-json = {
      url = "github:nix-community/all-cabal-json?ref=hackage";
      flake = false;
    };
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.dream2nix.flakeModuleBeta
      ];

      # Hack to make following work with IFD:
      # nix flake show --impure --allow-import-from-derivation
      systems =
        if builtins.hasAttr "currentSystem" builtins
        then [builtins.currentSystem]
        else inputs.nixpkgs.lib.systems.flakeExposed;

      dream2nix = {
        config.projectRoot = ./.;
      };
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
            fourmolu.enable = true;
          };
          tools = {
            fourmolu = pkgs.lib.mkForce (pkgs.writeShellScriptBin "fourmolu" ''
              ${pkgs.haskell.packages.ghc925.fourmolu_0_9_0_0}/bin/fourmolu -o -XQuasiQuotes -o -XTemplateHaskell -o -XTypeApplications -o -XImportQualifiedPost -o -XPatternSynonyms -o -XOverloadedRecordDot "$@"
            '');
          };
        };

        dream2nix.inputs.horth = {
          pname = "horth";
          source = (import inputs.nix-filter) {
            root = ./.;
            include = [
              "src"
              "app"
              "stack.yaml"
              "stack.yaml.lock"
              "horth.cabal"
              "package.yaml"
              "LICENSE"
            ];
          };
          projects.horth = {
            name = "horth";
            subsystem = "haskell";
            translator = "stack-lock";
            subsystemInfo.compiler = {
              name = "ghc";
              version = "9.4.3";
            };
          };
        };

        checks = let
          # Test if interpreted output is the same as compiled output
          testHorthFile = fname: let
            file = fname + ".horth";
          in
            pkgs.stdenvNoCC.mkDerivation {
              name = "horth-test-${fname}";
              src = ./examples;
              nativeBuildInputs = [
                pkgs.nasm
                pkgs.binutils
                pkgs.coreutils
                self'.packages.horth
              ];

              configurePhase = ''
                export LC_CTYPE=C.UTF-8
                export LC_ALL=C.UTF-8
                export LANG=C.UTF-8
              '';

              buildPhase = ''
                echo "Compiling ${file}..."
                horth compile --input ${file} --output out --format elf64
              '';

              checkPhase = ''
                echo "Pretty-printing ${file}.horth..."
                horth pretty --input ${file} > pretty.horth
                (diff ${file} pretty.horth) || (echo "Pretty:    FAIL" && exit 1)
                echo "Pretty:    PASS"
              '';

              installPhase = ''
                mkdir -p $out/bin
                cp out $out/bin/${fname}
              '';

              doInstallCheck = true;
              installCheckPhase = ''
                ./out
                echo "Execution: PASS"
              '';
            };
        in {
          hello = testHorthFile "hello";
          fac = testHorthFile "fac";
          mem = testHorthFile "mem";
          echo = testHorthFile "echo";
          combined =
            pkgs.runCommand "combined-test"
            {
              nativeBuildInputs =
                builtins.attrValues
                (pkgs.lib.attrsets.filterAttrs (name: _: name != "combined") self'.checks);
            } "touch $out";
        };

        devShells.default = let
          stackWithSystemGHC = pkgs.writeShellScriptBin "stack" ''
            ${pkgs.stack}/bin/stack --system-ghc --no-nix "$@"
          '';
        in
          pkgs.mkShell {
            shellHook = config.pre-commit.installationScript;
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
              pkgs.nasm
              pkgs.binutils
              pkgs.gdb
            ];
          };
        formatter = pkgs.alejandra;
      };
    };
}
