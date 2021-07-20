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

              hls-tactics-plugin = hsuper.hls-tactics-plugin.override {
                refinery = hself.refinery_0_3_0_0;
              };

              hiedb = hself.callCabal2nix "hiedb"
                (builtins.fetchTarball {
                  url =
                    "https://hackage.haskell.org/package/hiedb-0.4.0.0/hiedb-0.4.0.0.tar.gz";
                  sha256 =
                    "13jz8c46zfpf54ya2wsv4akhn0wcfc6qjazqsjfir5gpvsi7v8xr";
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
        ghc8105 = pkgs.hlsHpkgs "ghc8105";
        ghc901 = ghc901Config.tweakHpkgs (pkgs.hlsHpkgs "ghc901");

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
            buildInputs = [ gmp zlib ncurses capstone tracy (gen-hls-changelogs hpkgs) ]
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
          haskell-language-server-8105-dev = mkDevShell ghc8105;
          haskell-language-server-901-dev = mkDevShell ghc901;

          # hls package
          haskell-language-server = mkExe ghcDefault;
          haskell-language-server-884 = mkExe ghc884;
          haskell-language-server-8104 = mkExe ghc8104;
          haskell-language-server-8105 = mkExe ghc8105;
          haskell-language-server-901 = mkExe ghc901;
        };

        defaultPackage = packages.haskell-language-server;

        devShell = packages.haskell-language-server-dev;
      });
}
