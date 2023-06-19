{ pkgs, inputs }:

let
  disabledPlugins = [ "hls-stylish-haskell-plugin" ];

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

      Cabal = hself.Cabal_3_6_3_0;

      Cabal-syntax = hself.Cabal-syntax_3_8_1_0;

      ghc-lib-parser = hself.callCabal2nix "ghc-lib-parser" inputs.ghc-lib-parser-94 {};

      lsp = hself.callCabal2nix "lsp" inputs.lsp {};
      lsp-types = hself.callCabal2nix "lsp-types" inputs.lsp-types {};
      lsp-test = hself.callCabal2nix "lsp-test" inputs.lsp-test {};

      hlint = appendConfigureFlag (hself.callCabal2nix "hlint" inputs.hlint-35 {}) "-fghc-lib";

      hls-hlint-plugin = hself.callCabal2nixWithOptions "hls-hlint-plugin"
        ./plugins/hls-hlint-plugin
        (pkgs.lib.concatStringsSep " " [ "-fhlint34" "-fghc-lib" ]) { };

      OneTuple = overrideCabal hsuper.OneTuple (drv: {
        libraryHaskellDepends = drv.libraryHaskellDepends or [] ++ [
          hself.base-orphans
        ];
      });

      ormolu = hself.callCabal2nix "ormolu" inputs.ormolu-052 {};

      fourmolu = hself.callHackage "fourmolu" "0.10.1.0" {};

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [ "-f-stylishhaskell" ])
        { };

    });
in {
  inherit disabledPlugins;
  tweakHpkgs = hpkgs: hpkgs.extend hpkgsOverride;
}
