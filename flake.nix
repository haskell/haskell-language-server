# Maintaining this file:
#
#     - Bump the inputs version using `nix flake update`
#     - Edit `sourceDirs` to update the set of local packages
#
# For more details: https://nixos.wiki/wiki/Flakes
{
  description = "haskell language server flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    flake-utils.url = "github:numtide/flake-utils";
    pre-commit-hooks = {
      url = "github:cachix/pre-commit-hooks.nix";
    };
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      flake = false;
    };

    # List of hackage dependencies
    lsp = {
      url = "https://hackage.haskell.org/package/lsp-1.4.0.0/lsp-1.4.0.0.tar.gz";
      flake = false;
    };
    lsp-types = {
      url = "https://hackage.haskell.org/package/lsp-types-1.4.0.1/lsp-types-1.4.0.1.tar.gz";
      flake = false;
    };
    lsp-test = {
      url = "https://hackage.haskell.org/package/lsp-test-0.14.0.2/lsp-test-0.14.0.2.tar.gz";
      flake = false;
    };
    ghc-exactprint = {
      url = "https://hackage.haskell.org/package/ghc-exactprint-1.4.1/ghc-exactprint-1.4.1.tar.gz";
      flake = false;
    };
    constraints-extras = {
      url = "https://hackage.haskell.org/package/constraints-extras-0.3.2.1/constraints-extras-0.3.2.1.tar.gz";
      flake = false;
    };
    retrie = {
      url = "https://hackage.haskell.org/package/retrie-1.2.0.1/retrie-1.2.0.1.tar.gz";
      flake = false;
    };
    fourmolu = {
      url = "https://hackage.haskell.org/package/fourmolu-0.5.0.1/fourmolu-0.5.0.1.tar.gz";
      flake = false;
    };
    hlint = {
      url = "https://hackage.haskell.org/package/hlint-3.3.6/hlint-3.3.6.tar.gz";
      flake = false;
    };
    implicit-hie-cradle = {
      url = "https://hackage.haskell.org/package/implicit-hie-cradle-0.3.0.5/implicit-hie-cradle-0.3.0.5.tar.gz";
      flake = false;
    };
  };
  outputs =
    inputs@{ self, nixpkgs, flake-compat, flake-utils, pre-commit-hooks, gitignore, ... }:
    {
      overlay = final: prev:
        with prev;
        let
          haskellOverrides = hself: hsuper: {
            # we override mkDerivation here to apply the following
            # tweak to each haskell package:
            #   if the package is broken, then we disable its check and relax the cabal bounds;
            #   otherwise, we leave it unchanged.
            # hopefully, this could fix packages marked as broken by nix due to check failures
            # or the build failure because of tight cabal bounds
            mkDerivation = args:
              let
                broken = args.broken or false;
                check = args.doCheck or true;
                jailbreak = args.jailbreak or false;
              in hsuper.mkDerivation (args // {
                jailbreak = if broken then true else jailbreak;
                doCheck = if broken then false else check;
              });
          };
          gitignoreSource = (import gitignore { inherit lib; }).gitignoreSource;

          # List all subdirectories under `./plugins`, except `./plugins/default`
          pluginsDir = ./plugins;
          pluginSourceDirs = builtins.removeAttrs (lib.mapAttrs'
            (name: _: lib.nameValuePair name (pluginsDir + ("/" + name)))
            (builtins.readDir pluginsDir)) [ "default" ];

          # Source directories of our packages, should be consistent with cabal.project
          sourceDirs = {
            haskell-language-server = ./.;
            ghcide = ./ghcide;
            hls-graph = ./hls-graph;
            shake-bench = ./shake-bench;
            hie-compat = ./hie-compat;
            hls-plugin-api = ./hls-plugin-api;
            hls-test-utils = ./hls-test-utils;
          } // pluginSourceDirs;

          # Tweak our packages
          # Don't use `callHackage`, it requires us to override `all-cabal-hashes`
          tweaks = hself: hsuper:
            with haskell.lib; {
              # Patches don't apply
              github = overrideCabal hsuper.github (drv: { patches = []; });
              # GHCIDE requires hie-bios >=0.8 && <0.9.0
              hie-bios = hself.hie-bios_0_8_0;
              # We need an older version
              hiedb = hself.hiedb_0_4_1_0;

              lsp = hsuper.callCabal2nix "lsp" inputs.lsp {};
              lsp-types = hsuper.callCabal2nix "lsp-types" inputs.lsp-types {};
              lsp-test = hsuper.callCabal2nix "lsp-test" inputs.lsp-test {};

              implicit-hie-cradle = hself.callCabal2nix "implicit-hie-cradle" inputs.implicit-hie-cradle {};

              # https://github.com/NixOS/nixpkgs/issues/140774
              ormolu =
                if final.system == "aarch64-darwin"
                then overrideCabal hsuper.ormolu (_: { enableSeparateBinOutput = false; })
                else hsuper.ormolu;
            };

          hlsSources =
            builtins.mapAttrs (_: dir: gitignoreSource dir) sourceDirs;

          extended = hpkgs:
            (hpkgs.override (old: {
              overrides = lib.composeExtensions (old.overrides or (_: _: { }))
                haskellOverrides;
            })).extend (hself: hsuper:
              # disable all checks for our packages
              builtins.mapAttrs (_: drv: haskell.lib.dontCheck drv)
              (lib.composeExtensions
                (haskell.lib.packageSourceOverrides hlsSources) tweaks hself
                hsuper));

        in {
          inherit hlsSources;

          # Haskell packages extended with our packages
          hlsHpkgs = compiler: extended haskell.packages.${compiler};

          # Support of GenChangelogs.hs
          gen-hls-changelogs = hpkgs:
            let myGHC = hpkgs.ghcWithPackages (p: with p; [ github ]);
            in runCommand "gen-hls-changelogs" {
              passAsFile = [ "text" ];
              preferLocalBuild = true;
              allowSubstitutes = false;
              buildInputs = [ git myGHC ];
            } ''
              dest=$out/bin/gen-hls-changelogs
              mkdir -p $out/bin
              echo "#!${runtimeShell}" >> $dest
              echo "${myGHC}/bin/runghc ${./GenChangelogs.hs}" >> $dest
              chmod +x $dest
            '';
        };
    } // (flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-darwin" ])
    (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlay ];
          config = { allowBroken = true; };
        };

        # Pre-commit hooks to run stylish-haskell
        pre-commit-check = hpkgs: pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            stylish-haskell.enable = true;
            # use stylish-haskell with our target ghc
            stylish-haskell.entry = pkgs.lib.mkForce "${hpkgs.stylish-haskell}/bin/stylish-haskell --inplace";
            stylish-haskell.excludes = [
              # Ignored files
              "^Setup.hs$"
              "test/testdata/.*$"
              "test/data/.*$"
              "test/manual/lhs/.*$"
              "^hie-compat/.*$"
              "^plugins/hls-tactics-plugin/.*$"

              # Temporarily ignored files
              # Stylish-haskell (and other formatters) does not work well with some CPP usages in these files
              "^ghcide/src/Development/IDE/GHC/Compat.hs$"
              "^ghcide/src/Development/IDE/Plugin/CodeAction/ExactPrint.hs$"
              "^ghcide/src/Development/IDE/GHC/Compat/Core.hs$"
              "^ghcide/src/Development/IDE/Spans/Pragmas.hs$"
              "^ghcide/src/Development/IDE/LSP/Outline.hs$"
              "^plugins/hls-splice-plugin/src/Ide/Plugin/Splice.hs$"
              "^ghcide/test/exe/Main.hs$"
              "ghcide/src/Development/IDE/Core/Rules.hs"
              "^hls-test-utils/src/Test/Hls/Util.hs$"
            ];
          };
        };

        ghc901Config = (import ./configuration-ghc-901.nix) { inherit pkgs; };
        ghc921Config = (import ./configuration-ghc-921.nix) { inherit pkgs inputs; };

        # GHC versions
        ghcDefault = pkgs.hlsHpkgs ("ghc"
          + pkgs.lib.replaceStrings [ "." ] [ "" ]
          pkgs.haskellPackages.ghc.version);
        ghc884 = pkgs.hlsHpkgs "ghc884";
        ghc8107 = pkgs.hlsHpkgs "ghc8107";
        ghc901 = ghc901Config.tweakHpkgs (pkgs.hlsHpkgs "ghc901");
        ghc921 = ghc921Config.tweakHpkgs (pkgs.hlsHpkgs "ghc921");

        # For markdown support
        myst-parser = pkgs.python3Packages.callPackage ./myst-parser.nix {};
        sphinx_rtd_theme = pkgs.python3Packages.sphinx_rtd_theme.overrideAttrs (oldAttrs: {
          # For https://github.com/readthedocs/sphinx_rtd_theme/pull/1185, otherwise lists are broken locally
          src = pkgs.fetchFromGitHub {
            owner = "readthedocs";
            repo = "sphinx_rtd_theme";
            rev = "34f81daaf52466366c80003db293d50075c1b896";
            sha256 = "0rkrsvvqr6g2p3v5vq88jhfp5sd0r1jqjh3vc5y26jn30z8s4fkz";
          };
        });
        pythonWithPackages = pkgs.python3.withPackages (ps: [ps.sphinx myst-parser sphinx_rtd_theme ps.pip]);

        docs = pkgs.stdenv.mkDerivation {
          name = "hls-docs";
          src = pkgs.lib.sourceFilesBySuffices ./. [ ".py" ".rst" ".md" ".png" ".gif" ".svg" ".cabal" ];
          buildInputs = [ pythonWithPackages ];
          # -n gives warnings on missing link targets, -W makes warnings into errors
          buildPhase = ''cd docs; sphinx-build -n -W . $out'';
          dontInstall = true;
        };

        mkDevShell = hpkgs: cabalProject: with pkgs; mkShell {
          name = "haskell-language-server-dev-ghc${hpkgs.ghc.version}";
          # For theses tools packages, we use ghcDefault
          # This removes a rebuild with a different GHC version
          # Theses programs are tools, used as binary, independently of the
          # version of GHC.
          # The drawback of this approach is that our shell may pull two GHC
          # version in scope (the default one, and the one defined in
          # `hpkgs`.)
          # The advantage is that we won't have to rebuild theses tools (and
          # dependencies) with a recent GHC which may not be supported by
          # them.
          buildInputs = [
            # our compiling toolchain
            hpkgs.ghc
            pkgs.cabal-install
            # @guibou: I'm not sure hie-bios is needed
            ghcDefault.hie-bios
            # Dependencies needed to build some parts of hackage
            gmp zlib ncurses
            # Changelog tooling
            (gen-hls-changelogs ghcDefault)
            # For the documentation
            pythonWithPackages
            # @guibou: I'm not sure this is needed.
            hlint
            ghcDefault.opentelemetry-extra
            capstone tracy
            # ormolu
            # stylish-haskell
            ];


          shellHook = ''
            # @guibou: I'm not sure theses lines are needed
            export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib:${capstone}/lib
            export DYLD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib:${capstone}/lib
            export PATH=$PATH:$HOME/.local/bin

            # Enable the shell hooks
            ${(pre-commit-check ghcDefault).shellHook}

            # If the cabal project file is not the default one.
            # Print a warning and generate an alias.
            if [ ${cabalProject} != "cabal.project" ]
            then
              echo "Cabal won't be able to build your project without using the project file "${cabalProject}", such as:"
              echo "    cabal --project-file=${cabalProject}"
              echo "An alias "cabal_project" is available. Use it like:"
              echo "    cabal_project build"

              alias cabal_project='cabal --project-file=${cabalProject}'
            fi
          '';
        };

        # Create a development shell of hls project
        # See https://github.com/NixOS/nixpkgs/blob/5d4a430472cafada97888cc80672fab255231f57/pkgs/development/haskell-modules/make-package-set.nix#L319
        mkDevShellWithNixDeps = hpkgs: cabalProject:
          with pkgs;
          let simpleShell = mkDevShell hpkgs cabalProject;
          in
          hpkgs.shellFor {
            name = "haskell-language-server-dev-nix-ghc${hpkgs.ghc.version}";
            inherit (simpleShell) shellHook buildInputs;

            doBenchmark = true;
            packages = p:
              with builtins;
              map (name: p.${name}) (attrNames
              # Disable dependencies should not be part of the shell.
              (removeAttrs hlsSources (hpkgs.hlsDisabledPlugins or [])));

            src = null;
          };
        # Create a hls executable
        # Copied from https://github.com/NixOS/nixpkgs/blob/210784b7c8f3d926b7db73bdad085f4dc5d79418/pkgs/development/tools/haskell/haskell-language-server/withWrapper.nix#L16
        mkExe = hpkgs:
          with pkgs.haskell.lib;
          (enableSharedExecutables (overrideCabal hpkgs.haskell-language-server
            (_: {
              postInstall = ''
                remove-references-to -t ${hpkgs.shake.data} $out/bin/haskell-language-server
                remove-references-to -t ${hpkgs.js-jquery.data} $out/bin/haskell-language-server
                remove-references-to -t ${hpkgs.js-dgtable.data} $out/bin/haskell-language-server
                remove-references-to -t ${hpkgs.js-flot.data} $out/bin/haskell-language-server
              '';
            }))).overrideAttrs(old: {
              pname = old.pname + "-ghc${hpkgs.ghc.version}";
            });
      in with pkgs; rec {
        # Developement shell with only compiler
        simpleDevShells = {
          haskell-language-server-dev = mkDevShell ghcDefault "cabal.project";
          haskell-language-server-884-dev = mkDevShell ghc884 "cabal.project";
          haskell-language-server-8107-dev = mkDevShell ghc8107 "cabal.project";
          haskell-language-server-901-dev = mkDevShell ghc901 "cabal-ghc90.project";
          haskell-language-server-921-dev = mkDevShell ghc921 "cabal-ghc921.project";
        };

        # Developement shell, haskell packages are also provided by nix
        nixDevShells = {
          haskell-language-server-dev-nix = mkDevShellWithNixDeps ghcDefault "cabal.project";
          haskell-language-server-884-dev-nix = mkDevShellWithNixDeps ghc884 "cabal.project";
          haskell-language-server-8107-dev-nix = mkDevShellWithNixDeps ghc8107 "cabal.project";
          haskell-language-server-901-dev-nix = mkDevShellWithNixDeps ghc901 "cabal-ghc90.project";
          haskell-language-server-921-dev-nix = mkDevShellWithNixDeps ghc921 "cabal-ghc921.project";
        };

        allPackages = {
          haskell-language-server = mkExe ghcDefault;
          haskell-language-server-884 = mkExe ghc884;
          haskell-language-server-8107 = mkExe ghc8107;
          haskell-language-server-901 = mkExe ghc901;
          haskell-language-server-921 = mkExe ghc921;
        };

        devShells = simpleDevShells // nixDevShells;

        packages = allPackages // {
          # See https://github.com/NixOS/nix/issues/5591
          # nix flake cannot build a list/set of derivation in one command.
          # Using a linkFarmFromDrvs, I'm creating a unique entry point to
          # build all HLS versions.
          # This is used in CI to test and populate cache for packages
          # distributed using nix.
          all-haskell-language-server = linkFarmFromDrvs "all-haskell-language-server" (lib.unique (builtins.attrValues allPackages));

          # Same for all shells 
          # We try to build as much as possible, but not much shells are
          # working (especially on darwing), so this list is limited.
          all-nix-dev-shells = linkFarmFromDrvs "all-dev-shells" (builtins.map (shell: shell.inputDerivation) (lib.unique [nixDevShells.haskell-language-server-dev-nix]));

          all-simple-dev-shells = linkFarmFromDrvs "all-dev-shells" (builtins.map (shell: shell.inputDerivation) (lib.unique (builtins.attrValues simpleDevShells)));
          docs = docs;
        };

        defaultPackage = packages.haskell-language-server;

        devShell = devShells.haskell-language-server-dev;
      });
}
