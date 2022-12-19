{ pkgs, inputs }:

let
  disabledPlugins = [
    "hls-hlint-plugin"
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
      # ptr-poker breaks on MacOS without SSE2 optimizations
      # https://github.com/nikita-volkov/ptr-poker/issues/11
      ptr-poker = hself.callCabal2nix "ptr-poker" inputs.ptr-poker { };

      ghc-exactprint =
        hself.callCabal2nix "ghc-exactprint" inputs.ghc-exactprint-160 { };
      # Hlint is still broken
      hlint = doJailbreak (hself.callCabal2nix "hlint" inputs.hlint { });

      stylish-haskell = appendConfigureFlag  hsuper.stylish-haskell "-fghc-lib";

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [ "-fpedantic" "-f-hlint" ]) { };
    });
in {
  inherit disabledPlugins;
  tweakHpkgs = hpkgs: hpkgs.extend hpkgsOverride;
}
