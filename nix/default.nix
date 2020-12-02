{ sources ? import ./sources.nix }:
let
  overlay = _self: pkgs:
    let sharedOverrides = {
        overrides = _self: super: {
            mkDerivation = args: super.mkDerivation (args //
                {
                    # skip running tests for Hackage packages
                    doCheck = args.pname != "ghcide" && args.pname != "haskell-language-server";
                    # relax upper bounds
                    jailbreak = args.pname != "jailbreak-cabal";
                });
            };
            };
        extended = haskellPackages:
          haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides {
            haskell-language-server = ../.;
            ghcide = ../ghcide;
            hie-compat = ../ghcide/hie-compat;
            hls-plugin-api = ../hls-plugin-api;
            hls-tactics-plugin = ../plugins/tactics;
            hls-hlint-plugin = ../plugins/hls-hlint-plugin;
          });
        in
        {
        inherit (import sources.gitignore { inherit (pkgs) lib; }) gitignoreSource;
        ourHaskell = pkgs.haskell // {
            packages = pkgs.haskell.packages // {
                # relax upper bounds on ghc 8.10.x versions (and skip running tests)
                ghc8101 = extended (pkgs.haskell.packages.ghc8101.override sharedOverrides);
                ghc8102 = extended (pkgs.haskell.packages.ghc8102.override sharedOverrides);
            };
        };
        };

in import sources.nixpkgs
{ overlays = [ overlay ] ; config = {allowBroken = true;}; }
