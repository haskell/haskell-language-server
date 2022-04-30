# nix version of cabal-ghc902.project
{ pkgs, inputs }:

let
  disabledPlugins = [ "hls-brittany-plugin" "hls-stylish-haskell-plugin" ];

  hpkgsOverride = hself: hsuper:
    with pkgs.haskell.lib; {
      hlsDisabledPlugins = disabledPlugins;

      # Hlint is still broken
      hlint = doJailbreak (hself.callCabal2nix "hlint" inputs.hlint-34 { });

      hls-hlint-plugin = hself.callCabal2nixWithOptions "hls-hlint-plugin"
        ./plugins/hls-hlint-plugin
        (pkgs.lib.concatStringsSep " " [ "-fhlint34" ]) { };

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [ "-f-brittany" "-f-stylishhaskell" ])
        { };

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
