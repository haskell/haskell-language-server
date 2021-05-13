# Maintaining this file:
#
#     - Bump the inputs version using `nix flake update`
#     - Edit `sourceDirs` to update the set of local packages
#
# For more details: https://nixos.wiki/wiki/Flakes
{
  description = "haskell language server flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      flake = false;
    };
  };
  outputs =
    { self, nixpkgs, flake-compat, flake-utils, pre-commit-hooks, gitignore }:
    {
      overlay = final: prev:
        with prev;
        let
          haskellOverrides = {
            overrides = hself: hsuper: {
              # we override mkDerivation here to apply the following
              # tweak to each haskell package:
              #   if the package is broken, then we disable its check and relax the cabal bounds;
              #   otherwise, we leave it unchanged.
              # hopefully, this could fix packages marked as broken by nix due to check failures
              # or the build failure because of tight cabal bounds
              mkDerivation = args:
                let
                  broken = args.broken or false;
                  check = args.doCheck or true;
                  jailbreak = args.jailbreak or false;
                in hsuper.mkDerivation (args // {
                  jailbreak = if broken then true else jailbreak;
                  doCheck = if broken then false else check;
                });
            };
          };
          gitignoreSource = (import gitignore { inherit lib; }).gitignoreSource;

          sourceDirs = {
            haskell-language-server = ./.;
            ghcide = ./ghcide;
            hls-graph = ./hls-graph;
            shake-bench = ./shake-bench;
            hie-compat = ./hie-compat;
            hls-plugin-api = ./hls-plugin-api;
            hls-test-utils = ./hls-test-utils;
            hls-brittany-plugin = ./plugins/hls-brittany-plugin;
            hls-stylish-haskell-plugin = ./plugins/hls-stylish-haskell-plugin;
            hls-class-plugin = ./plugins/hls-class-plugin;
            hls-haddock-comments-plugin = ./plugins/hls-haddock-comments-plugin;
            hls-eval-plugin = ./plugins/hls-eval-plugin;
            hls-explicit-imports-plugin = ./plugins/hls-explicit-imports-plugin;
            hls-refine-imports-plugin = ./plugins/hls-refine-imports-plugin;
            hls-hlint-plugin = ./plugins/hls-hlint-plugin;
            hls-retrie-plugin = ./plugins/hls-retrie-plugin;
            hls-splice-plugin = ./plugins/hls-splice-plugin;
            hls-tactics-plugin = ./plugins/hls-tactics-plugin;
          };

          hlsSources =
            builtins.mapAttrs (_: dir: gitignoreSource dir) sourceDirs;
        in {
          inherit hlsSources;

          # haskellPackages extended with hls packages
          hlsHaskellPackages = with haskell.lib;
            (haskellPackages.override haskellOverrides).extend (self: super:
              # disable check for ghcide and hls
              builtins.mapAttrs (name: drv:
                if name == "ghcide" || name == "haskell-language-server" then
                  dontCheck drv
                else
                  drv) (packageSourceOverrides hlsSources self super));

          # Support of GenChangelogs.hs
          gen-hls-changelogs =
            let myGHC = haskellPackages.ghcWithPackages (p: with p; [ github ]);
            in runCommand "gen-hls-changelogs" {
              passAsFile = [ "text" ];
              preferLocalBuild = true;
              allowSubstitutes = false;
              buildInputs = [ git myGHC ];
            } ''
              dest=$out/bin/gen-hls-changelogs
              mkdir -p $out/bin
              echo "#!${runtimeShell}" >> $dest
              echo "${myGHC}/bin/runghc ${./GenChangelogs.hs}" >> $dest
              chmod +x $dest
            '';
        };
    } // (flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ])
    (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
          config = { allowBroken = true; };
        };
        hlsPackages = p:
          with builtins;
          map (name: p.${name}) (attrNames pkgs.hlsSources);
        pre-commit-check = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            stylish-haskell.enable = true;
            stylish-haskell.excludes = [
              # Ignored files
              "^Setup.hs$"
              "test/testdata/.*$"
              "test/data/.*$"
              "test/manual/lhs/.*$"
              "^hie-compat/.*$"
              "^plugins/hls-tactics-plugin/.*$"

              # Temporarily ignored files
              # Stylish-haskell (and other formatters) does not work well with some CPP usages in these files
              "^ghcide/src/Development/IDE/GHC/Compat.hs$"
              "^plugins/hls-splice-plugin/src/Ide/Plugin/Splice.hs$"
              "^ghcide/test/exe/Main.hs$"
              "ghcide/src/Development/IDE/Core/Rules.hs"
              "^hls-test-utils/src/Test/Hls/Util.hs$"
            ];
          };
        };
      in with pkgs; rec {

        packages = with pkgs.haskell.lib; {
          haskell-language-server =
            justStaticExecutables hlsHaskellPackages.haskell-language-server;
          ghcide = justStaticExecutables hlsHaskellPackages.ghcide;
        };

        apps = {
          haskell-language-server = flake-utils.lib.mkApp {
            drv = packages.haskell-language-server;
            exePath = "/bin/haskell-language-server";
          };
          ghcide = flake-utils.lib.mkApp {
            drv = packages.ghcide;
            exePath = "/bin/ghcide";
          };
        };

        defaultPackage = packages.haskell-language-server;
        defaultApp = apps.haskell-language-server;

        devShell = hlsHaskellPackages.shellFor {
          doBenchmark = true;
          packages = hlsPackages;
          buildInputs = [ gmp zlib ncurses capstone tracy gen-hls-changelogs ]
            ++ (with haskellPackages; [
              cabal-install
              hlint
              ormolu
              stylish-haskell
              opentelemetry-extra
            ]);

          src = null;
          shellHook = ''
            export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib:${capstone}/lib
            export DYLD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib:${capstone}/lib
            export PATH=$PATH:$HOME/.local/bin
            ${pre-commit-check.shellHook}
          '';
        };
      });
}
