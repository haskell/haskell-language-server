# Maintaining this file:
#
#     - Bump the inputs version using `nix flake update`
#     - Edit `sourceDirs` to update the set of local packages
#
# For more details: https://nixos.wiki/wiki/Flakes
{
  description = "haskell language server flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
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
          haskellOverrides = hself: hsuper: {
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
          gitignoreSource = (import gitignore { inherit lib; }).gitignoreSource;

          # List all subdirectories under `./plugins`, except `./plugins/default`
          pluginsDir = ./plugins;
          pluginSourceDirs = builtins.removeAttrs (lib.mapAttrs'
            (name: _: lib.nameValuePair name (pluginsDir + ("/" + name)))
            (builtins.readDir pluginsDir)) [ "default" ];

          # Source directories of our packages, should be consistent with cabal.project
          sourceDirs = {
            haskell-language-server = ./.;
            ghcide = ./ghcide;
            hls-graph = ./hls-graph;
            shake-bench = ./shake-bench;
            hie-compat = ./hie-compat;
            hls-plugin-api = ./hls-plugin-api;
            hls-test-utils = ./hls-test-utils;
          } // pluginSourceDirs;

          # Tweak our packages
          # Don't use `callHackage`, it requires us to override `all-cabal-hashes`
          tweaks = hself: hsuper:
            with haskell.lib; {

              ghc-api-compat = hself.callCabal2nix "ghc-api-compat"
                (pkgs.fetchFromGitHub {
                  owner = "hsyl20";
                  repo = "ghc-api-compat";
                  rev = "8fee87eac97a538dbe81ff1ab18cff10f2f9fa15";
                  sha256 = "byehvdxQxhNk5ZQUXeFHjAZpAze4Ct9261ro4c5acZk=";
                }) { };

              lsp = hself.callCabal2nix "lsp"
                (builtins.fetchTarball {
                  url = "https://hackage.haskell.org/package/lsp-1.2.0.1/lsp-1.2.0.1.tar.gz";
                  sha256 = "1lhzsraiw11ldxvxn8ax11hswpyzsvw2da2qmp3p6fc9rfpz4pj5";
                }) { };

              lsp-types = hself.callCabal2nix "lsp-types"
                (builtins.fetchTarball {
                  url = "https://hackage.haskell.org/package/lsp-types-1.3.0.0/lsp-types-1.3.0.0.tar.gz";
                  sha256 = "0qajyyj2d51daa4y0pqaa87n4nny0i920ivvzfnrk9gq9386iac7";
                }) { };

              lsp-test = hself.callCabal2nix "lsp-test"
                (builtins.fetchTarball {
                  url = "https://hackage.haskell.org/package/lsp-test-0.14.0.1/lsp-test-0.14.0.1.tar.gz";
                  sha256 = "10lnyg7nlbd3ymgvjjlrkfndyy7ay9cwnsk684p08k2gzlric4yq";
                }) { };
            };

          hlsSources =
            builtins.mapAttrs (_: dir: gitignoreSource dir) sourceDirs;

          extended = hpkgs:
            (hpkgs.override (old: {
              overrides = lib.composeExtensions (old.overrides or (_: _: { }))
                haskellOverrides;
            })).extend (hself: hsuper:
              # disable all checks for our packages
              builtins.mapAttrs (_: drv: haskell.lib.dontCheck drv)
              (lib.composeExtensions
                (haskell.lib.packageSourceOverrides hlsSources) tweaks hself
                hsuper));

        in {
          inherit hlsSources;

          # Haskell packages extended with our packages
          hlsHpkgs = compiler: extended haskell.packages.${compiler};

          # Support of GenChangelogs.hs
          gen-hls-changelogs = hpkgs:
            let myGHC = hpkgs.ghcWithPackages (p: with p; [ github ]);
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

        # Pre-commit hooks to run stylish-haskell
        pre-commit-check = hpkgs: pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            stylish-haskell.enable = true;
            # use stylish-haskell with our target ghc
            stylish-haskell.entry = pkgs.lib.mkForce "${hpkgs.stylish-haskell}/bin/stylish-haskell --inplace";
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

        ghc901Config = (import ./configuration-ghc-901.nix) { inherit pkgs; };

        # GHC versions
        ghcDefault = pkgs.hlsHpkgs ("ghc"
          + pkgs.lib.replaceStrings [ "." ] [ "" ]
          pkgs.haskellPackages.ghc.version);
        ghc884 = pkgs.hlsHpkgs "ghc884";
        ghc8104 = pkgs.hlsHpkgs "ghc8104";
        ghc901 = ghc901Config.tweakHpkgs (pkgs.hlsHpkgs "ghc901");

        # For markdown support
        myst-parser = pkgs.python3Packages.callPackage ./myst-parser.nix {};
        sphinx_rtd_theme = pkgs.python3Packages.sphinx_rtd_theme.overrideAttrs (oldAttrs: {
          # For https://github.com/readthedocs/sphinx_rtd_theme/pull/1185, otherwise lists are broken locally
          src = pkgs.fetchFromGitHub {
            owner = "readthedocs";
            repo = "sphinx_rtd_theme";
            rev = "34f81daaf52466366c80003db293d50075c1b896";
            sha256 = "0rkrsvvqr6g2p3v5vq88jhfp5sd0r1jqjh3vc5y26jn30z8s4fkz";
          };
        });
        pythonWithPackages = pkgs.python3.withPackages (ps: [ps.sphinx myst-parser sphinx_rtd_theme ps.pip]);

        docs = pkgs.stdenv.mkDerivation {
          name = "hls-docs";
          src = pkgs.lib.sourceFilesBySuffices ./docs [ ".py" ".rst" ".md" ".png" ".gif" ".svg" ];
          buildInputs = [ pythonWithPackages ];
          # -n gives warnings on missing link targets, -W makes warnings into errors
          buildPhase = ''sphinx-build -n -W . $out'';
          dontInstall = true;
        };

        # Create a development shell of hls project
        # See https://github.com/NixOS/nixpkgs/blob/5d4a430472cafada97888cc80672fab255231f57/pkgs/development/haskell-modules/make-package-set.nix#L319
        mkDevShell = hpkgs:
          with pkgs;
          hpkgs.shellFor {
            doBenchmark = true;
            packages = p:
              with builtins;
              map (name: p.${name}) (attrNames
                (if hpkgs.ghc.version == "9.0.1" then
                  removeAttrs hlsSources ghc901Config.disabledPlugins
                else
                  hlsSources));
            buildInputs = [ gmp zlib ncurses capstone tracy (gen-hls-changelogs hpkgs) pythonWithPackages ]
              ++ (with hpkgs; [
                cabal-install
                hlint
                # ormolu
                # stylish-haskell
                opentelemetry-extra
              ]);

            src = null;
            shellHook = ''
              export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib:${capstone}/lib
              export DYLD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib:${capstone}/lib
              export PATH=$PATH:$HOME/.local/bin
              ${if hpkgs.ghc.version != "9.0.1" then (pre-commit-check hpkgs).shellHook else ""}
            '';
          };
        # Create a hls executable
        # Copied from https://github.com/NixOS/nixpkgs/blob/210784b7c8f3d926b7db73bdad085f4dc5d79418/pkgs/development/tools/haskell/haskell-language-server/withWrapper.nix#L16
        mkExe = hpkgs:
          with pkgs.haskell.lib;
          justStaticExecutables (overrideCabal hpkgs.haskell-language-server
            (_: {
              postInstall = ''
                remove-references-to -t ${hpkgs.ghc} $out/bin/haskell-language-server
                remove-references-to -t ${hpkgs.shake.data} $out/bin/haskell-language-server
                remove-references-to -t ${hpkgs.js-jquery.data} $out/bin/haskell-language-server
                remove-references-to -t ${hpkgs.js-dgtable.data} $out/bin/haskell-language-server
                remove-references-to -t ${hpkgs.js-flot.data} $out/bin/haskell-language-server
              '';
            }));
      in with pkgs; rec {

        packages = {
          # dev shell
          haskell-language-server-dev = mkDevShell ghcDefault;
          haskell-language-server-884-dev = mkDevShell ghc884;
          haskell-language-server-8104-dev = mkDevShell ghc8104;
          haskell-language-server-8105-dev = builtins.throw "GHC 8.10.5 is not available in nixpkgs";
          haskell-language-server-901-dev = mkDevShell ghc901;

          # hls package
          haskell-language-server = mkExe ghcDefault;
          haskell-language-server-884 = mkExe ghc884;
          haskell-language-server-8104 = mkExe ghc8104;
          haskell-language-server-8105 = builtins.throw "GHC 8.10.5 is not available in nixpkgs";
          haskell-language-server-901 = mkExe ghc901;

          # docs
          docs = docs;
        };

        defaultPackage = packages.haskell-language-server;

        devShell = packages.haskell-language-server-dev;
      });
}
