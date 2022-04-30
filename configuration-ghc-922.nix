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

      fourmolu = hself.callCabal2nix "fourmolu" inputs.fourmolu { };
      ghc-exactprint =
        hself.callCabal2nix "ghc-exactprint" inputs.ghc-exactprint-150 { };
      constraints-extras =
        hself.callCabal2nix "constraints-extras" inputs.constraints-extras { };
      retrie = hself.callCabal2nix "retrie" inputs.retrie { };

      # ptr-poker breaks on MacOS without SSE2 optimizations
      # https://github.com/nikita-volkov/ptr-poker/issues/11
      ptr-poker = hself.callCabal2nix "ptr-poker" inputs.ptr-poker { };

      # Hlint is still broken
      hlint = doJailbreak (hself.callCabal2nix "hlint" inputs.hlint { });

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [
          "-f-brittany"
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
