{ sources ? import ./sources.nix }:
let
  overlay = _self: pkgs:
    let sharedOverrides = {
        overrides = _self: super: {
            mkDerivation = args: super.mkDerivation (args //
                {
                    # skip running tests for Hackage packages
                    doCheck =
                        # but not for ghcide
                        args.version == "0";
                    # relax upper bounds
                    jailbreak = args.pname != "jailbreak-cabal";
                });
            };
            };
        in
        {
        inherit (import sources.gitignore { inherit (pkgs) lib; }) gitignoreSource;
        ourHaskell = pkgs.haskell // {
            packages = pkgs.haskell.packages // {
                # relax upper bounds on ghc 8.10.x versions (and skip running tests)
                ghc8101 = pkgs.haskell.packages.ghc8101.override sharedOverrides;
                ghc8102 = pkgs.haskell.packages.ghc8102.override sharedOverrides;
            };
        };
        };

in import sources.nixpkgs
{ overlays = [ overlay ] ; config = {allowBroken = true;}; }
