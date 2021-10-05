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
    let
      dependent-sum-src = pkgs.fetchFromGitHub {
        owner = "anka-213";
        repo = "dependent-sum";
        rev = "8cf4c7fbc3bfa2be475a17bb7c94a1e1e9a830b5";
        sha256 = "WtxTB6ufTZC6SxOtGSfhlO4mY0y9eWejMSa0yUJ7dHQ=";
      };
    in {

      blaze-textual = hself.callCabal2nix "blaze-textual"
        (builtins.fetchTarball {
          url = "https://hackage.haskell.org/package/blaze-textual-0.2.2.1/blaze-textual-0.2.2.1.tar.gz";
          sha256 = "1nyhc9mrnxsl21ksnpp0ryki4wgk49r581yy504g2gjq6x3bkb59";
        }) { };

      czipwith = hself.callCabal2nix "czipwith" (pkgs.fetchFromGitHub {
        owner = "mithrandi";
        repo = "czipwith";
        rev = "b6245884ae83e00dd2b5261762549b37390179f8";
        sha256 = "2uSoGyrxT/OstRcpx55kwP4JwjPbWLxD72LajeyQV0E=";
      }) { };

      hie-bios = hself.callCabal2nix "hie-bios"
        (builtins.fetchTarball {
          url = "https://hackage.haskell.org/package/hie-bios-0.7.6/hie-bios-0.7.6.tar.gz";
          sha256 = "0w4rhy4b3jnci9m27l79c8n28wl56x49bmhdn7pvf88mx9srjcvq";
        }) { };

      th-extras = hself.callCabal2nix "th-extras" (pkgs.fetchFromGitHub {
        owner = "anka-213";
        repo = "th-extras";
        rev = "57a97b4df128eb7b360e8ab9c5759392de8d1659";
        sha256 = "Qtha1ge/C0L+uFcV2dZ5xpG59DCxQT7LuK/OYfiM4Pk=";
      }) { };

      dependent-sum =
        hself.callCabal2nix "dependent-sum" "${dependent-sum-src}/dependent-sum"
        { };

      dependent-sum-template = hself.callCabal2nix "dependent-sum-template"
        "${dependent-sum-src}/dependent-sum-template" { };

      hlint = hself.hlint_3_3_1;

      ghc-lib-parser = hself.ghc-lib-parser_9_0_1_20210324;

      ghc-lib-parser-ex = hself.ghc-lib-parser-ex_9_0_0_4;

      ormolu = hself.ormolu_0_2_0_0;

      diagrams-core = hself.diagrams-core_1_5_0;

      diagrams-lib = hself.diagrams-lib_1_4_4;

      dual-tree = hself.dual-tree_0_2_3_0;

      monoid-extras = hself.monoid-extras_0_6;

      # Released on hackage, but not in nixpkgs yet
      operational = hself.callCabal2nix "operational" (pkgs.fetchFromGitHub {
        owner = "HeinrichApfelmus";
        repo = "operational";
        rev = "2b33e0055066cf92a302ee2c32058dfa44ac8882";
        sha256 = "sha256-nwB4vssm4wUTkVryjQVb3peOwR6js7vdekkbaWedHNI=";
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
