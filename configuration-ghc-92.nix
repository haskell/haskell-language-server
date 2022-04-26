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
      # haddock seems broken on aarch64-darwin for some reason
      ghc-trace-events = if pkgs.stdenv.isDarwin && pkgs.stdenv.isAarch64 then dontHaddock hsuper.ghc-trace-events else hsuper.ghc-trace-events;
      parsers = if pkgs.stdenv.isDarwin && pkgs.stdenv.isAarch64 then dontHaddock hsuper.parsers else hsuper.parsers;
      prettyprinter-ansi-terminal = if pkgs.stdenv.isDarwin && pkgs.stdenv.isAarch64 then dontHaddock hsuper.prettyprinter-ansi-terminal else hsuper.prettyprinter-ansi-terminal;

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
        hself.callCabal2nix "ghc-exactprint" inputs.ghc-exactprint-150 { };
      # Hlint is still broken
      hlint = doJailbreak (hself.callCabal2nix "hlint" inputs.hlint { });

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [ "-fpedantic" "-f-hlint" ]) { };
    });
in {
  inherit disabledPlugins;
  tweakHpkgs = hpkgs: hpkgs.extend hpkgsOverride;
}
