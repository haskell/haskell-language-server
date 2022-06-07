{ pkgs }: with pkgs;
let

  gitignore = builtins.fetchGit {
    url = "https://github.com/hercules-ci/gitignore.nix";
    rev = "bff2832ec341cf30acb3a4d3e2e7f1f7b590116a";
    #  = "sha256-kekOlTlu45vuK2L9nq8iVN17V3sB0WWPqTTW3a2SQG0=";
  };

  inputs = with pkgs.haskell.packages.mwb.ghc922; {
    ptr-poker = ptr-poker;
    ghc-exactprint-150 = builtins.fetchTarball "https://hackage.haskell.org/package/ghc-exactprint-1.5.0/ghc-exactprint-1.5.0.tar.gz";
    hlint = hlint;
    hie-bios = builtins.fetchTarball "https://hackage.haskell.org/package/hie-bios-0.9.1/hie-bios-0.9.1.tar.gz";
    lsp = builtins.fetchTarball "https://hackage.haskell.org/package/lsp-1.4.0.0/lsp-1.4.0.0.tar.gz";
    lsp-types = builtins.fetchTarball "https://hackage.haskell.org/package/lsp-types-1.4.0.1/lsp-types-1.4.0.1.tar.gz";
    lsp-test = builtins.fetchTarball "https://hackage.haskell.org/package/lsp-test-0.14.0.2/lsp-test-0.14.0.2.tar.gz";
    implicit-hie-cradle = builtins.fetchTarball "https://hackage.haskell.org/package/implicit-hie-cradle-0.3.0.5/implicit-hie-cradle-0.3.0.5.tar.gz";
  };

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
        # Library profiling is disabled as it causes long compilation time
        # on our CI jobs. Nix users are free tor revert this anytime.
        enableLibraryProfiling = false;
        doHaddock = false;
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
      # Patches don't apply
      github = overrideCabal hsuper.github (drv: { patches = []; });
      # GHCIDE requires hie-bios ^>=0.9.1
      hie-bios = hself.callCabal2nix "hie-bios" inputs.hie-bios {};

      lsp = hsuper.callCabal2nix "lsp" inputs.lsp {};
      lsp-types = hsuper.callCabal2nix "lsp-types" inputs.lsp-types {};
      lsp-test = hsuper.callCabal2nix "lsp-test" inputs.lsp-test {};

      implicit-hie-cradle = hself.callCabal2nix "implicit-hie-cradle" inputs.implicit-hie-cradle {};

      # https://github.com/NixOS/nixpkgs/issues/140774
      ormolu =
        if pkgs.system == "aarch64-darwin"
        then overrideCabal hsuper.ormolu (_: { enableSeparateBinOutput = false; })
        else hsuper.ormolu;
    };

  hlsSources = sourceDirs;

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

  hlsHpkgs = extended pkgs.haskell.packages.mwb.ghc922;

  ghc922Config = (import ./configuration-ghc-92.nix) { inherit pkgs inputs; };

  mkExe = hpkgs:
    with pkgs.haskell.lib;
    (enableSharedExecutables (overrideCabal hpkgs.haskell-language-server
      (_: {
        postInstall = ''
          remove-references-to -t ${hpkgs.shake.data} $out/bin/haskell-language-server
          remove-references-to -t ${hpkgs.js-jquery.data} $out/bin/haskell-language-server
          remove-references-to -t ${hpkgs.js-dgtable.data} $out/bin/haskell-language-server
          remove-references-to -t ${hpkgs.js-flot.data} $out/bin/haskell-language-server
        '';
      }))).overrideAttrs(old: {
        pname = old.pname + "-ghc${hpkgs.ghc.version}";
      });

  supportedGHCs = let
    ghcVersion = "ghc" + (pkgs.lib.replaceStrings ["."] [""] pkgs.haskellPackages.ghc.version);
    cases = {
      ghc922 = ghc922Config.tweakHpkgs hlsHpkgs;
    };
    in { default = cases."${ghcVersion}"; } // cases;


  ghc922 = supportedGHCs.ghc922;

in {
  haskell-language-server = mkExe ghc922;
}
