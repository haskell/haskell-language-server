{ pkgs, inputs }:

let
  disabledPlugins = [ "hls-stylish-haskell-plugin" ];

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
      Cabal = hself.Cabal_3_6_3_0;

      Cabal-syntax = hself.Cabal-syntax_3_8_1_0;

      ghc-lib-parser = hsuper.ghc-lib-parser_9_4_5_20230430;

      lsp = hself.callCabal2nix "lsp" inputs.lsp {};
      lsp-types = hself.callCabal2nix "lsp-types" inputs.lsp-types {};
      lsp-test = hself.callCabal2nix "lsp-test" inputs.lsp-test {};

      hlint = appendConfigureFlag (hself.callCabal2nix "hlint" inputs.hlint-35 {}) "-fghc-lib";

      hls-hlint-plugin = hself.callCabal2nixWithOptions "hls-hlint-plugin"
        ./plugins/hls-hlint-plugin
        (pkgs.lib.concatStringsSep " " [ "-fhlint34" "-fghc-lib" ]) { };

      OneTuple = overrideCabal hsuper.OneTuple (drv: {
        libraryHaskellDepends = drv.libraryHaskellDepends or [] ++ [
          hself.base-orphans
        ];
      });

      ormolu = hself.callCabal2nix "ormolu" inputs.ormolu-052 {};

      fourmolu = hsuper.fourmolu_0_10_1_0;

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [ "-f-stylishhaskell" ])
        { };

    });
in {
  inherit disabledPlugins;
  tweakHpkgs = hpkgs: hpkgs.extend hpkgsOverride;
}
