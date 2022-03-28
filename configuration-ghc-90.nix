# nix version of cabal-ghc901.project
{ pkgs }:

let
  disabledPlugins = [
    "hls-brittany-plugin"
    "hls-stylish-haskell-plugin"
  ];

  hpkgsOverride = hself: hsuper:
    with pkgs.haskell.lib;
    {
      hlsDisabledPlugins = disabledPlugins;

      fourmolu = hself.fourmolu_0_4_0_0;
      primitive-extras = hself.primitive-extras_0_10_1_2;

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [
          "-f-brittany"
          "-f-stylishhaskell"
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
