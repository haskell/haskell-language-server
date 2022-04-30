# nix version of cabal-ghc901.project
{ pkgs, inputs }:

let
  disabledPlugins = [
    "hls-brittany-plugin"
    "hls-hlint-plugin"
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

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [ ]) { };

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
