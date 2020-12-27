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
        gitignoreSource = (import sources.gitignore { inherit (pkgs) lib; }).gitignoreSource;
        extend = haskellPackages:
          (haskellPackages.override sharedOverrides).extend (pkgs.haskell.lib.packageSourceOverrides {
            ghcide = gitignoreSource ../.;
            hie-compat = gitignoreSource ../hie-compat;
            shake-bench = gitignoreSource ../shake-bench;
          });
        in
        {
        inherit gitignoreSource;
        ourHaskell = pkgs.haskell // {
            packages = pkgs.haskell.packages // {
                # relax upper bounds on ghc 8.10.x versions (and skip running tests)
                ghc8101 = extend pkgs.haskell.packages.ghc8101;
                ghc8102 = extend pkgs.haskell.packages.ghc8102;
            };
        };
        };

in import sources.nixpkgs
{ overlays = [ overlay ] ; config = {allowBroken = true;}; }
