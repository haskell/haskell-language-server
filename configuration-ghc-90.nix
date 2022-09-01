{ pkgs, inputs }:

let
  disabledPlugins = [ "hls-brittany-plugin" "hls-stylish-haskell-plugin" ];

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
      # ptr-poker breaks on MacOS without SSE2 optimizations
      # https://github.com/nikita-volkov/ptr-poker/issues/11
      ptr-poker = hself.callCabal2nix "ptr-poker" inputs.ptr-poker { };

      ghc-lib = hself.ghc-lib_9_2_2_20220307;
      ghc-lib-parser = hself.ghc-lib-parser_9_2_4_20220729;
      ghc-lib-parser-ex = hself.ghc-lib-parser-ex_9_2_0_4;

      Cabal = hself.Cabal_3_6_3_0;
      ormolu = hself.ormolu_0_5_0_1;
      fourmolu = hself.fourmolu_0_6_0_0;
      # Hlint is still broken
      hlint = doJailbreak (hself.callCabal2nix "hlint" inputs.hlint-34 { });

      hls-hlint-plugin = hself.callCabal2nixWithOptions "hls-hlint-plugin"
        ./plugins/hls-hlint-plugin
        (pkgs.lib.concatStringsSep " " [ "-fhlint34" "-fghc-lib" ]) { };

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [ "-f-brittany" "-f-stylishhaskell" ])
        { };
    });
in {
  inherit disabledPlugins;
  tweakHpkgs = hpkgs: hpkgs.extend hpkgsOverride;
}
