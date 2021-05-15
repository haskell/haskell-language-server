{ sources ? import ./sources.nix }:
let
  nix-pre-commit-hooks = (import (builtins.fetchTarball "https://github.com/cachix/pre-commit-hooks.nix/tarball/87fb108527c7865ebee2ffe9af6154cb761ec9e0/" + "/nix/") { sources = sources; }).packages;
  overlay = _self: pkgs:
    let
        sharedOverrides = {
          overrides = _self: super: {
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
              in super.mkDerivation (args // {
                jailbreak = if broken then true else jailbreak;
                doCheck = if broken then false else check;
              });
          };
        };
        ourSources = {
            haskell-language-server = gitignoreSource ../.;
            ghcide = gitignoreSource ../ghcide;
            hls-graph = gitignoreSource ../hls-graph;
            shake-bench = gitignoreSource ../shake-bench;
            hie-compat = gitignoreSource ../hie-compat;
            hls-plugin-api = gitignoreSource ../hls-plugin-api;
            hls-test-utils = gitignoreSource ../hls-test-utils;
            hls-brittany-plugin = gitignoreSource ../plugins/hls-brittany-plugin;
            hls-stylish-haskell-plugin = gitignoreSource ../plugins/hls-stylish-haskell-plugin;
            hls-fourmolu-plugin = gitignoreSource ../plugins/hls-fourmolu-plugin;
            hls-class-plugin = gitignoreSource ../plugins/hls-class-plugin;
            hls-haddock-comments-plugin = gitignoreSource ../plugins/hls-haddock-comments-plugin;
            hls-eval-plugin = gitignoreSource ../plugins/hls-eval-plugin;
            hls-explicit-imports-plugin = gitignoreSource ../plugins/hls-explicit-imports-plugin;
            hls-refine-imports-plugin = gitignoreSource ../plugins/hls-refine-imports-plugin;
            hls-hlint-plugin = gitignoreSource ../plugins/hls-hlint-plugin;
            hls-retrie-plugin = gitignoreSource ../plugins/hls-retrie-plugin;
            hls-splice-plugin = gitignoreSource ../plugins/hls-splice-plugin;
            hls-tactics-plugin = gitignoreSource ../plugins/hls-tactics-plugin;
            hls-floskell-plugin = gitignoreSource ../plugins/hls-floskell-plugin;
            hls-pragmas-plugin = gitignoreSource ../plugins/hls-pragmas-plugin;
        };
        gitignoreSource = (import sources.gitignore { inherit (pkgs) lib; }).gitignoreSource;
        extended = haskellPackages:
          haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides ourSources);
        in
        {
        inherit gitignoreSource;
        inherit ourSources;

        gen-hls-changelogs = with pkgs;
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
              echo "${myGHC}/bin/runghc ${../GenChangelogs.hs}" >> $dest
              chmod +x $dest
            '';

        ourHaskell = pkgs.haskell // {
            packages = pkgs.haskell.packages // {
                # relax upper bounds on ghc 8.10.x versions (and skip running tests)
                ghc8102 = extended (pkgs.haskell.packages.ghc8102.override sharedOverrides);
                ghc8103 = extended (pkgs.haskell.packages.ghc8103.override sharedOverrides);
                ghc8104 = extended (pkgs.haskell.packages.ghc8104.override sharedOverrides);
            };
        };
        };

in (import sources.nixpkgs
  {
    overlays = [ overlay ];
    config = {allowBroken = true;};
  }) // {
    pre-commit-check = nix-pre-commit-hooks.run {
      src = ./.;
      # If your hooks are intrusive, avoid running on each commit with a default_states like this:
      # default_stages = ["manual" "push"];
      hooks = {
        stylish-haskell.enable = true;
        stylish-haskell.excludes = [
          # Ignored files
          "^Setup.hs$" "test/testdata/.*$" "test/data/.*$" "test/manual/lhs/.*$" "^hie-compat/.*$" "^plugins/hls-tactics-plugin/.*$"

          # Temporarily ignored files
          # Stylish-haskell (and other formatters) does not work well with some CPP usages in these files
          "^ghcide/src/Development/IDE/GHC/Compat.hs$" "^plugins/hls-splice-plugin/src/Ide/Plugin/Splice.hs$" "^ghcide/test/exe/Main.hs$" "ghcide/src/Development/IDE/Core/Rules.hs" "^hls-test-utils/src/Test/Hls/Util.hs$"
        ];
      };
    };
  }
