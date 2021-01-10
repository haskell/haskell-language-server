{ sources ? import ./sources.nix }:
let
  overlay = _self: pkgs:
    let sharedOverrides = {
        overrides = _self: super: {
            mkDerivation = args: super.mkDerivation (args //
                {
                    # skip running tests for Hackage packages
                    doCheck = args.pname == "ghcide" || args.pname == "haskell-language-server";
                    # relax upper bounds
                    jailbreak = args.pname != "jailbreak-cabal";
                });
            };
            };
        gitignoreSource = (import sources.gitignore { inherit (pkgs) lib; }).gitignoreSource;
        extended = haskellPackages:
          haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
            haskell-language-server = gitignoreSource ../.;
            ghcide = gitignoreSource ../ghcide;
            shake-bench = gitignoreSource ../shake-bench;
            hie-compat = gitignoreSource ../hie-compat;
            hls-plugin-api = gitignoreSource ../hls-plugin-api;
            hls-exactprint-utils = gitignoreSource ../hls-exactprint-utils;
            hls-class-plugin = gitignoreSource ../plugins/hls-class-plugin;
            hls-haddock-comments-plugin = gitignoreSource ../plugins/hls-haddock-comments-plugin;
            hls-eval-plugin = gitignoreSource ../plugins/hls-eval-plugin;
            hls-explicit-imports-plugin = gitignoreSource ../plugins/hls-explicit-imports-plugin;
            hls-hlint-plugin = gitignoreSource ../plugins/hls-hlint-plugin;
            hls-retrie-plugin = gitignoreSource ../plugins/hls-retrie-plugin;
            hls-splice-plugin = gitignoreSource ../plugins/hls-splice-plugin;
            hls-tactics-plugin = gitignoreSource ../plugins/tactics;
          });
        in
        {
        inherit gitignoreSource;
        ourHaskell = pkgs.haskell // {
            packages = pkgs.haskell.packages // {
                # relax upper bounds on ghc 8.10.x versions (and skip running tests)
                ghc8101 = extended (pkgs.haskell.packages.ghc8101.override sharedOverrides);
                ghc8102 = extended (pkgs.haskell.packages.ghc8102.override sharedOverrides);
                ghc8103 = extended (pkgs.haskell.packages.ghc8103.override sharedOverrides);
            };
        };
        };

in import sources.nixpkgs
{ overlays = [ overlay ] ; config = {allowBroken = true;}; }
