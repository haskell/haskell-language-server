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
      apply-refact = hsuper.apply-refact_0_12_0_0;
      tagged = hself.callHackage "tagged" "0.8.7" { };
      primitive = hself.callHackage "primitive" "0.8.0.0" { };
      unix-compat = hself.callCabal2nix "unix-compat" inputs.haskell-unix-compat { };
      MonadRandom = hself.callHackage "MonadRandom" "0.6" { };
      hiedb = hself.callCabal2nix "hiedb" inputs.haskell-hiedb { };
      hie-bios = hself.callCabal2nix "hie-bios" inputs.haskell-hie-bios { };
      implicit-hie-cradle = hself.callCabal2nix "implicit-hie-cradle" inputs.haskell-implicit-hie-cradle { };
      ghc-exactprint = hself.callCabal2nix "ghc-exactprint" inputs.haskell-ghc-exactprint { };

      # ptr-poker breaks on MacOS without SSE2 optimizations
      # https://github.com/nikita-volkov/ptr-poker/issues/11
      ptr-poker = hself.callCabal2nix "ptr-poker" inputs.ptr-poker { };

      ormolu = hself.ormolu_0_5_3_0;

      # TODO: smunix: nix fails to build fourmolu-0.13 from Hackage with these errors:
      #   tar: */fourmolu/0.13.0.0/fourmolu.json: Not found in archive
      #   tar: */fourmolu/0.13.0.0/fourmolu.cabal: Not found in archive
      #   tar: Exiting with failure status due to previous errors
      # As an alternative, we could build directly from github:fourmolu. How do people
      # feel about this?
      fourmolu = hself.callHackage "fourmolu" "0.12.0.0" {};

      stylish-haskell = appendConfigureFlag  hsuper.stylish-haskell "-fghc-lib";

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
