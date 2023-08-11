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
      apply-refact = hsuper.apply-refact_0_13_0_0;

      Cabal-syntax = hself.Cabal-syntax_3_8_1_0;

      ghc-lib-parser = hsuper.ghc-lib-parser_9_4_5_20230430;

      hiedb = hself.callCabal2nix "hiedb" inputs.hiedb {};

      hlint = appendConfigureFlag (hself.callCabal2nix "hlint" inputs.hlint-35 {}) "-fghc-lib";

      ormolu = hself.callCabal2nix "ormolu" inputs.ormolu-052 {};

      fourmolu = hsuper.fourmolu_0_10_1_0;

      stylish-haskell = hsuper.stylish-haskell_0_14_4_0;

      hie-bios = hself.callCabal2nix "hie-bios" inputs.haskell-hie-bios { };

      implicit-hie-cradle = hself.callCabal2nix "implicit-hie-cradle" inputs.haskell-implicit-hie-cradle { };

      lsp = hself.callCabal2nix "lsp" inputs.lsp {};
      lsp-types = hself.callCabal2nix "lsp-types" inputs.lsp-types {};
      lsp-test = dontCheck (hself.callCabal2nix "lsp-test" inputs.lsp-test {});

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [ "-fpedantic" "-f-hlint" ]) { };

    });
in {
  inherit disabledPlugins;
  tweakHpkgs = hpkgs: hpkgs.extend hpkgsOverride;
}
