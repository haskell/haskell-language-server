{ sources ? import ./sources.nix }:
let
  nix-pre-commit-hooks = (import (builtins.fetchTarball "https://github.com/cachix/pre-commit-hooks.nix/tarball/master/" + "/nix/") { sources = sources; }).packages;
  overlay = _self: pkgs:
    let
        sharedOverrides = {
          overrides = _self: super: {
            mkDerivation = args:
              let
                broken = args.broken or false;
                check = args.doCheck or true;
              in super.mkDerivation (args // { jailbreak = broken; doCheck = if broken then false else check; });
          };
        };
        ourSources = {
            haskell-language-server = gitignoreSource ../.;
            ghcide = gitignoreSource ../ghcide;
            shake-bench = gitignoreSource ../shake-bench;
            hie-compat = gitignoreSource ../hie-compat;
            hls-plugin-api = gitignoreSource ../hls-plugin-api;
            hls-brittany-plugin = gitignoreSource ../plugins/hls-brittany-plugin;
            hls-class-plugin = gitignoreSource ../plugins/hls-class-plugin;
            hls-haddock-comments-plugin = gitignoreSource ../plugins/hls-haddock-comments-plugin;
            hls-eval-plugin = gitignoreSource ../plugins/hls-eval-plugin;
            hls-explicit-imports-plugin = gitignoreSource ../plugins/hls-explicit-imports-plugin;
            hls-hlint-plugin = gitignoreSource ../plugins/hls-hlint-plugin;
            hls-retrie-plugin = gitignoreSource ../plugins/hls-retrie-plugin;
            hls-splice-plugin = gitignoreSource ../plugins/hls-splice-plugin;
            hls-tactics-plugin = gitignoreSource ../plugins/hls-tactics-plugin;
        };
        gitignoreSource = (import sources.gitignore { inherit (pkgs) lib; }).gitignoreSource;
        extended = haskellPackages:
          haskellPackages.extend (pkgs.haskell.lib.packageSourceOverrides ourSources);
        in
        {
        inherit gitignoreSource;
        inherit ourSources;
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
        stylish-haskell.excludes = [ "^Setup.hs$" "test/testdata/.*$" "test/data/.*$" "^hie-compat/.*$" ];
      };
    };
  }
