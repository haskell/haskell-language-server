# nix version of cabal-ghc901.project
{ pkgs, inputs }:

let
  disabledPlugins = [
    "hls-brittany-plugin"
    "hls-stylish-haskell-plugin"
    "hls-hlint-plugin"
    "hls-haddock-comments-plugin"
    "hls-tactics-plugin"
    # That one is not technically a plugin, but by putting it in this list, we
    # get it removed from the top level list of requirement and it is not pull
    # in the nix shell.
    "shake-bench"
  ];

  hpkgsOverride = hself: hsuper:
    with pkgs.haskell.lib;
    {
      hlsDisabledPlugins = disabledPlugins;

      fourmolu = hself.callCabal2nix "fourmolu" inputs.fourmolu {};
      primitive-extras = hself.primitive-extras_0_10_1_2;
      ghc-exactprint = hself.callCabal2nix "ghc-exactprint" inputs.ghc-exactprint {};
      constraints-extras = hself.callCabal2nix "constraints-extras" inputs.constraints-extras {};
      retrie = hself.callCabal2nix "retrie" inputs.retrie {};

      # Hlint is still broken
      hlint = doJailbreak (hself.callCabal2nix "hlint" inputs.hlint {});
      hiedb = hself.hiedb_0_4_1_0;

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [
          "-f-brittany"
          "-f-stylishHaskell"
          "-f-hlint"
          "-f-haddockComments"
          "-f-tactics"
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
