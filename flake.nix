{
  description = "Description for the project";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/haskell-updates";
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    pre-commit-hooks-nix = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    haskell-flake = {
      url = "github:srid/haskell-flake";
    };
  };

  outputs = inputs @ {
    self,
    flake-parts,
    ...
  }:
    flake-parts.lib.mkFlake {inherit inputs;} {
      imports = [
        inputs.treefmt-nix.flakeModule
        inputs.pre-commit-hooks-nix.flakeModule
        inputs.haskell-flake.flakeModule
        inputs.devshell.flakeModule
      ];
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin"];
      perSystem = {
        config,
        self',
        inputs',
        pkgs,
        system,
        ...
      }: {
        # Per-system attributes can be defined here. The self' and inputs'
        # module parameters provide easy access to attributes of the same
        # system.

        # Equivalent to  inputs'.nixpkgs.legacyPackages.hello;
        treefmt.programs = {
          alejandra.enable = true;
          cabal-fmt.enable = true;
          fourmolu = {
            enable = true;
            package = pkgs.haskellPackages.fourmolu;
          };
        };
        treefmt.projectRootFile = "flake.nix";
        pre-commit.settings = {
          hooks = {
            treefmt.enable = true;
            typos = {
              enable = true;
              settings.ignored-words = ["wheres"];
            };
            commitizen.enable = true;
            editorconfig-checker.enable = true;
          };
        };
        haskellProjects.default = {
          basePackages = pkgs.haskell.packages.ghc910;
          autoWire = ["checks" "apps" "packages"];
          defaults.devShell.tools = hp: {
            inherit (hp) haskell-language-server;
            inherit (pkgs) cabal-install;
          };
          devShell = {
            tools = hp:
              with hp; {
                haskell-dap = haskell-dap;
              };
          };
          otherOverlays = [
            (hself: hsuper: {
              primitive-unlifted = pkgs.haskell.lib.doJailbreak hsuper.primitive-unlifted;
              extensions = pkgs.haskell.lib.dontCheck hsuper.extensions;
              hw-prim = pkgs.haskell.lib.dontCheck hsuper.hw-prim;
              stan = pkgs.haskell.lib.dontCheck (hself.callHackageDirect {
                pkg = "stan";
                ver = "0.1.3.0";
                sha256 = "sha256-DHlK5Q6r/N7lRXgDMHJtwqDJi71EcTmaZKDRnpBTzrE=";
              } {});
            })
          ];
        };
        haskellProjects.ghcHackage = {
          basePackages = pkgs.haskell.packages.ghc963;
          autoWire = ["devShells" "checks" "apps" "packages"];
          defaults.devShell.tools = hp: {inherit (hp) cabal-install haskell-language-server;};
          devShell = {
            tools = hp:
              with hp; {
                haskell-dap = haskell-dap;
              };
          };
          otherOverlays = [
          ];
        };
        devshells.default = {
          devshell = {
            packagesFrom = [config.haskellProjects.default.outputs.devShell];
            startup.pre-commit.text = config.pre-commit.installationScript;
          };
        };
      };
      flake = {
        # The usual flake attributes can be defined here, including system-
        # agnostic ones like nixosModule and system-enumerating ones, although
        # those are more easily expressed in perSystem.
      };
    };
}
