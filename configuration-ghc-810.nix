{ pkgs, inputs }:

let
  disabledPlugins = [
    "hls-brittany-plugin"
    "hls-hlint-plugin"
    "hls-stylish-haskell-plugin"
    "hls-haddock-comments-plugin"
    "hls-tactics-plugin"
    # That one is not technically a plugin, but by putting it in this list, we
    # get it removed from the top level list of requirement and it is not pull
    # in the nix shell.
    "shake-bench"
  ];

  hpkgsOverride = hself: hsuper:
    with pkgs.haskell.lib; {
      hlsDisabledPlugins = disabledPlugins;

      ghc-lib = hself.ghc-lib_8_10_7_20220219;
      ghc-lib-parser = hself.ghc-lib-parser_8_10_7_20220219;

      fourmolu = hself.callCabal2nix "fourmolu" inputs.fourmolu-0300 { };

      stylish-haskell = hself.callCabal2nixWithOptions "stylish-haskell"
        inputs.stylish-haskell-01220 (pkgs.lib.concatStringsSep " " [ ]) { };

      aeson = hself.callCabal2nixWithOptions "aeson" inputs.aeson-1520
        (pkgs.lib.concatStringsSep " " [ ]) { };

      brittany = hself.callCabal2nixWithOptions "brittany" inputs.brittany-01312
        (pkgs.lib.concatStringsSep " " [ ]) { };

      hls-hlint-plugin = hself.callCabal2nixWithOptions "hls-hlint-plugin"
        ./plugins/hls-hlint-plugin
        (pkgs.lib.concatStringsSep " " [ "-f-hlint34" "-fhyphenation" ]) { };

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [
          "-fpedantic"
          "-f-hlint"
          "-f-fourmolu"
          "-f-ormolu"
        ]) { };

      # YOLO
      mkDerivation = args:
        hsuper.mkDerivation (args // {
          jailbreak = true;
          doCheck = false;
        });
    };
in {
  inherit disabledPlugins;
  tweakHpkgs = hpkgs: hpkgs.extend hpkgsOverride;
}
