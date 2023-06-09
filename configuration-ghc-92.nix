{ pkgs, inputs }:

let
  disabledPlugins = [
    # That one is not technically a plugin, but by putting it in this list, we
    # get it removed from the top level list of requirement and it is not pull
    # in the nix shell.
    "shake-bench"
  ];

  hpkgsOverride = hself: hsuper:
    with pkgs.haskell.lib;
    {
      hlsDisabledPlugins = disabledPlugins;
      # YOLO
      mkDerivation = args:
        hsuper.mkDerivation (args // {
          jailbreak = true;
          doCheck = false;
        });
    } // (builtins.mapAttrs (_: drv: disableLibraryProfiling drv) {
      apply-refact = hsuper.apply-refact_0_12_0_0;

      # ptr-poker breaks on MacOS without SSE2 optimizations
      # https://github.com/nikita-volkov/ptr-poker/issues/11
      ptr-poker = hself.callCabal2nix "ptr-poker" inputs.ptr-poker { };

      Cabal-syntax = hself.Cabal-syntax_3_8_1_0;

      ghc-lib-parser = hself.callCabal2nix "ghc-lib-parser" inputs.ghc-lib-parser-94 {};

      hlint = appendConfigureFlag (hself.callCabal2nix "hlint" inputs.hlint-35 {}) "-fghc-lib";

      ormolu = hself.callCabal2nix "ormolu" inputs.ormolu-052 {};

      fourmolu = hself.callHackage "fourmolu" "0.10.1.0" {};

      stylish-haskell = appendConfigureFlag  hsuper.stylish-haskell "-fghc-lib";

      hie-bios = hself.callCabal2nix "hie-bios" inputs.haskell-hie-bios { };

      implicit-hie-cradle = hself.callCabal2nix "implicit-hie-cradle" inputs.haskell-implicit-hie-cradle { };

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [ "-fpedantic" "-f-hlint" ]) { };

    });
in {
  inherit disabledPlugins;
  tweakHpkgs = hpkgs: hpkgs.extend hpkgsOverride;
}
