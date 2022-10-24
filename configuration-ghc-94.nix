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

      cereal = hsuper.callHackage "cereal" "0.5.8.3" {};
      base-compat = hsuper.callHackage "base-compat" "0.12.2" {};
      base-compat-batteries = hsuper.callHackage "base-compat-batteries" "0.12.2" {};
      hashable = hsuper.callHackage "hashable" "1.4.1.0" {};
      primitive = hsuper.callHackage "primitive" "0.7.4.0" {};
      ghc-check = hsuper.callHackage "ghc-check" "0.5.0.8" {};
      lens = hsuper.callHackage "lens" "5.2" {};
      integer-logarithms = hsuper.callHackage "integer-logarithms" "1.0.3.1" {};
      hiedb = hsuper.callHackage "hiedb" "0.4.2.0" {};
      hie-bios = hsuper.callHackage "hie-bios" "0.11.0" {};
      lsp = hsuper.callCabal2nix "lsp" inputs.lsp {};
      lsp-types = hsuper.callCabal2nix "lsp-types" inputs.lsp-types {};

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [ "-fpedantic" "-f-hlint" ]) { };
    });
in {
  inherit disabledPlugins;
  tweakHpkgs = hpkgs: hpkgs.extend hpkgsOverride;
}
