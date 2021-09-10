# nix version of cabal-ghc901.project
{ pkgs }:

let
  disabledPlugins = [
    "hls-tactics-plugin"
    "hls-brittany-plugin"
    "hls-stylish-haskell-plugin"
    "hls-fourmolu-plugin"
    "hls-splice-plugin"
    "hls-class-plugin"
    "hls-refine-imports-plugin"
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

      # we need add ghc-api-compat to build depends,
      # since its condition tree is not evaluated under ghc 9
      hiedb = addBuildDepend hsuper.hiedb hself.ghc-api-compat;

      blaze-textual = hself.callCabal2nix "blaze-textual"
        (pkgs.fetchFromGitHub {
          owner = "jwaldmann";
          repo = "blaze-textual";
          rev = "d8ee6cf80e27f9619d621c936bb4bda4b99a183f";
          sha256 = "C0dIzf64fBaY8mlhMm1kCQC5Jc1wKBtNO2Y24k7YPUw=";
        }) { };

      czipwith = hself.callCabal2nix "czipwith" (pkgs.fetchFromGitHub {
        owner = "mithrandi";
        repo = "czipwith";
        rev = "b6245884ae83e00dd2b5261762549b37390179f8";
        sha256 = "2uSoGyrxT/OstRcpx55kwP4JwjPbWLxD72LajeyQV0E=";
      }) { };

      hie-bios = hself.callCabal2nix "hie-bios" (pkgs.fetchFromGitHub {
        owner = "jneira";
        repo = "hie-bios";
        rev = "9b1445ab5efcabfad54043fc9b8e50e9d8c5bbf3";
        sha256 = "8ct7t3xIxIAoC+f8VO5e5+QKrd5L5Zu1eButSaE+1Uk=";
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

      operational = hself.callCabal2nix "operational" (pkgs.fetchFromGitHub {
        owner = "HeinrichApfelmus";
        repo = "operational";
        rev = "16e19aaf34e286f3d27b3988c61040823ec66537";
        sha256 = "P+aocEcqCN8klnW3IMrmIqq6ztBZJxk4sBp1ewN6YaA=";
      }) { };

      diagrams-core = hself.diagrams-core_1_5_0;

      diagrams-lib = hself.diagrams-lib_1_4_4;

      dual-tree = hself.dual-tree_0_2_3_0;

      monoid-extras = hself.monoid-extras_0_6;

      # Re-generate HLS drv excluding some plugins
      haskell-language-server =
        hself.callCabal2nixWithOptions "haskell-language-server" ./.
        (pkgs.lib.concatStringsSep " " [
          "-f-brittany"
          "-f-class"
          "-f-fourmolu"
          "-f-splice"
          "-f-stylishhaskell"
          "-f-tactic"
          "-f-refineImports"
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
