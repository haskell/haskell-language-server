{ sources ? import ./sources.nix }:
let
  overlay = selfPkgs: pkgs:
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

        ghc8102-infotable-profiling = pkgs.callPackage "${sources.ghcs-nix}/ghc.nix" {
            version = "8.10.2.20210110";
            src = pkgs.fetchgit {
              url = "https://gitlab.haskell.org/mpickering/ghc.git";
              rev = "57d6fb77529e85ca6eae18d283aadaf0966340de";
              sha256 = "0c4zg14j3p5ra0g2cj0jnqhk5br9ljf403395j867vxkvrpbc0cp";
              fetchSubmodules = true;
            };
            bootPkgs = pkgs.haskell.packages.ghc884;
            buildLlvmPackages = pkgs.llvmPackages;
            werror = false;
            sphinx = null;
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
        callGhcPackage = pkgs.buildPackages.newScope {
                      haskellLib = pkgs.haskell.lib;
                      overrides = pkgs.haskell.packageOverrides;
                      };
        in
        {
        inherit gitignoreSource;
        ourHaskell = pkgs.haskell // {
            compiler = pkgs.haskell.compiler // {
              ghc8102-infotable-profiling = ghc8102-infotable-profiling;
                        };
            packages = pkgs.haskell.packages // {
                # relax upper bounds on ghc 8.10.x versions (and skip running tests)
                ghc8101 = extended (pkgs.haskell.packages.ghc8101.override sharedOverrides);
                ghc8102 = extended (pkgs.haskell.packages.ghc8102.override sharedOverrides);
                ghc8103 = extended (pkgs.haskell.packages.ghc8103.override sharedOverrides);
                ghc8102-infotable-profiling = callGhcPackage "${<nixpkgs>}/pkgs/development/haskell-modules" {
                        buildHaskellPackages = selfPkgs.buildPackages.ourHaskell.packages.ghc8102-infotable-profiling;
                        ghc = selfPkgs.buildPackages.ourHaskell.compiler.ghc8102-infotable-profiling;
                        compilerConfig = callGhcPackage "${<nixpkgs>}/pkgs/development/haskell-modules/configuration-ghc-8.10.x.nix" { };
                                        };

            };
        };
        };

in import sources.nixpkgs
{ overlays = [ overlay ] ; config = {allowBroken = true;}; }
