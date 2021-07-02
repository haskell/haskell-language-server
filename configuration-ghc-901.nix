# nix version of cabal-ghc901.project
{ pkgs }:

let
  removePluginPackages = hpkgs:
    removeAttrs hpkgs [
      "hls-tactics-plugin"
      "hls-brittany-plugin"
      "hls-stylish-haskell-plugin"
      "hls-fourmolu-plugin"
      "hls-splice-plugin"
      "hls-ormolu-plugin"
    ];
  hpkgsOverride = hself: hsuper:
    with pkgs.haskell.lib;
    let
      lsp-src = pkgs.fetchFromGitHub {
        owner = "anka-213";
        repo = "lsp";
        rev = "tag-ghc-9.0.1-without-pr-326";
        sha256 = "lW/EdBnvKPLE2+CGE/grIekOu+U/Wh6zMCN4xhJDtPY=";
      };

      dependent-sum-src = pkgs.fetchFromGitHub {
        owner = "anka-213";
        repo = "dependent-sum";
        rev = "8cf4c7fbc3bfa2be475a17bb7c94a1e1e9a830b5";
        sha256 = "WtxTB6ufTZC6SxOtGSfhlO4mY0y9eWejMSa0yUJ7dHQ=";
      };
      hlint_3_3_1-src = builtins.fetchTarball {
        url =
          "https://hackage.haskell.org/package/hlint-3.3.1/hlint-3.3.1.tar.gz";
        sha256 = "03vb4s4w8k5vp5cjzg7m2bsinbjz1j2aqx4q06syq4r5vb3ai5yq";
      };
    in {
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

      ghc-api-compat = hself.callCabal2nix "ghc-api-compat"
        (pkgs.fetchFromGitHub {
          owner = "hsyl20";
          repo = "ghc-api-compat";
          rev = "8fee87eac97a538dbe81ff1ab18cff10f2f9fa15";
          sha256 = "byehvdxQxhNk5ZQUXeFHjAZpAze4Ct9261ro4c5acZk=";
        }) { };

      th-extras = hself.callCabal2nix "th-extras" (pkgs.fetchFromGitHub {
        owner = "anka-213";
        repo = "th-extras";
        rev = "57a97b4df128eb7b360e8ab9c5759392de8d1659";
        sha256 = "Qtha1ge/C0L+uFcV2dZ5xpG59DCxQT7LuK/OYfiM4Pk=";
      }) { };

      lsp = hself.callCabal2nix "lsp" "${lsp-src}/lsp" { };
      lsp-types = hself.callCabal2nix "lsp-types" "${lsp-src}/lsp-types" { };
      lsp-test = hself.callCabal2nix "lsp-test" "${lsp-src}/lsp-test" { };

      dependent-sum =
        hself.callCabal2nix "dependent-sum" "${dependent-sum-src}/dependent-sum"
        { };

      hlint = hself.callCabal2nix "hlint" hlint_3_3_1-src { };

      ghc-lib-parser = hself.ghc-lib-parser_9_0_1_20210324;

      ghc-lib-parser-ex = hself.ghc-lib-parser-ex_9_0_0_4;

      # Disable plugins
      haskell-language-server =
        appendConfigureFlags hsuper.haskell-language-server [
          "-brittany"
          "-eval"
          "-fourmolu"
          "-ormolu"
          "-splice"
          "-stylishhaskell"
          "-tactic"
          "-refineImports"
        ];
    };
in { tweakHpkgs = hpkgs: (removePluginPackages hpkgs).extend hpkgsOverride; }
