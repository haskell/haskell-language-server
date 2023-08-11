{ pkgs, inputs }:

let
  disabledPlugins = [
    # That one is not technically a plugin, but by putting it in this list, we
    # get it removed from the top level list of requirement and it is not pull
    # in the nix shell.
    "shake-bench"
    "hls-retrie-plugin"
    "hls-splice-plugin"
    "hls-class-plugin"
    "hls-rename-plugin"
    "hls-gadt-plugin"
    "hls-refactor-plugin"
  ];

  hpkgsOverride = hself: hsuper:
    with pkgs.haskell.lib;
    {
      hlsDisabledPlugins = disabledPlugins;

      # Override for all derivation
      # If they are considered as broken, we just disable jailbreak and hope for the best
      mkDerivation = args:
        hsuper.mkDerivation (args //
          {
            jailbreak = true;
            broken = false;
            doCheck = false;
          });
      apply-refact = hsuper.apply-refact_0_13_0_0;
      tagged = hsuper.tagged_0_8_7;
      primitive = hsuper.primitive_0_8_0_0;
      MonadRandom = hsuper.MonadRandom_0_6;
      hie-bios = hself.callCabal2nix "hie-bios" inputs.haskell-hie-bios { };
      hlint = hself.callCabal2nix "hlint" inputs.hlint-36 {};
      implicit-hie-cradle = hself.callCabal2nix "implicit-hie-cradle" inputs.haskell-implicit-hie-cradle { };

      fourmolu = hself.callCabal2nix "fourmolu" inputs.fourmolu-012 {};

      ghc-lib-parser-ex = hsuper.ghc-lib-parser-ex_9_6_0_0;

      ormolu = hself.callCabal2nix "ormolu" inputs.ormolu-07 {};

      stylish-haskell = hself.callCabal2nix "stylish-haskell" inputs.stylish-haskell-0145 {};

      lsp = hself.callCabal2nix "lsp" inputs.lsp {};
      lsp-types = hself.callCabal2nix "lsp-types" inputs.lsp-types {};
      lsp-test = dontCheck (hself.callCabal2nix "lsp-test" inputs.lsp-test {});

      hiedb = hself.callCabal2nix "hiedb" inputs.hiedb {};

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        # Pedantic cannot be used due to -Werror=unused-top-binds
        # Check must be disabled due to some missing required files
        (pkgs.lib.concatStringsSep " " [ "--no-check" "-f-pedantic" "-f-hlint" "-f-refactor" "-f-retrie" "-f-class" "-f-gadt" "-f-splice" "-f-rename" ]) { };
  };
in {
  inherit disabledPlugins;
  tweakHpkgs = hpkgs: hpkgs.extend hpkgsOverride;
}
