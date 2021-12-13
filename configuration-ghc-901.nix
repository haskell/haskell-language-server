# nix version of cabal-ghc901.project
{ pkgs }:

let
  disabledPlugins = [
    "hls-tactics-plugin"
    "hls-brittany-plugin"
    "hls-stylish-haskell-plugin"
    "hls-fourmolu-plugin"
    "hls-class-plugin"
  ];

  hpkgsOverride = hself: hsuper:
    with pkgs.haskell.lib;
    {

      # Released on hackage, but not in nixpkgs yet
      primitive-extras = hself.callCabal2nix "primitive-extras" (pkgs.fetchFromGitHub {
        owner = "metrix-ai";
        repo = "primitive-extras";
        rev = "c758d7366b99d85889cb13425fc0140879f8b936";
        sha256 = "sha256-vTT7svbM7IkhyxYx2xQ8p1ptoYe+ndcMN5+j9qx++7E=";
      }) { };

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [
          "-f-brittany"
          "-f-class"
          "-f-fourmolu"
          "-f-stylishhaskell"
          "-f-tactic"
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
