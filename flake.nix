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
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      flake = false;
    };

    # cabal hashes contains all the version for different haskell packages, to update:
    # nix flake lock --update-input all-cabal-hashes-unpacked
    all-cabal-hashes-unpacked = {
      url = "github:commercialhaskell/all-cabal-hashes/current-hackage";
      flake = false;
    };

    # List of hackage dependencies
    lsp = {
      url = "https://hackage.haskell.org/package/lsp-1.6.0.0/lsp-1.6.0.0.tar.gz";
      flake = false;
    };
    lsp-types = {
      url = "https://hackage.haskell.org/package/lsp-types-1.6.0.0/lsp-types-1.6.0.0.tar.gz";
      flake = false;
    };
    lsp-test = {
      url = "https://hackage.haskell.org/package/lsp-test-0.14.1.0/lsp-test-0.14.1.0.tar.gz";
      flake = false;
    };
    ghc-exactprint-160 = {
      url = "https://hackage.haskell.org/package/ghc-exactprint-1.6.0/ghc-exactprint-1.6.0.tar.gz";
      flake = false;
    };
    ghc-exactprint-150 = {
      url = "https://hackage.haskell.org/package/ghc-exactprint-1.5.0/ghc-exactprint-1.5.0.tar.gz";
      flake = false;
    };
    ghc-exactprint = {
      url = "https://hackage.haskell.org/package/ghc-exactprint-1.4.1/ghc-exactprint-1.4.1.tar.gz";
      flake = false;
    };
    ghc-check = {
      url = "https://hackage.haskell.org/package/ghc-check-0.5.0.8/ghc-check-0.5.0.8.tar.gz";
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
    fourmolu-0300 = {
      url = "https://hackage.haskell.org/package/fourmolu-0.3.0.0/fourmolu-0.3.0.0.tar.gz";
      flake = false;
    };
    aeson-1520= {
      url = "https://hackage.haskell.org/package/aeson-1.5.2.0/aeson-1.5.2.0.tar.gz";
      flake = false;
    };
    brittany-01312 = {
      url = "https://hackage.haskell.org/package/brittany-0.13.1.2/brittany-0.13.1.2.tar.gz";
      flake = false;
    };
    hlint = {
      url = "https://hackage.haskell.org/package/hlint-3.3.6/hlint-3.3.6.tar.gz";
      flake = false;
    };
    hlint-34 = {
      url = "https://hackage.haskell.org/package/hlint-3.4/hlint-3.4.tar.gz";
      flake = false;
    };
    ptr-poker = {
      url = "https://hackage.haskell.org/package/ptr-poker-0.1.2.8/ptr-poker-0.1.2.8.tar.gz";
      flake = false;
    };
    stylish-haskell = {
      url = "https://hackage.haskell.org/package/stylish-haskell-0.14.2.0/stylish-haskell-0.14.2.0.tar.gz";
      flake = false;
    };
    implicit-hie-cradle = {
      url = "https://hackage.haskell.org/package/implicit-hie-cradle-0.3.0.5/implicit-hie-cradle-0.3.0.5.tar.gz";
      flake = false;
    };
    hie-bios = {
      url = "https://hackage.haskell.org/package/hie-bios-0.11.0/hie-bios-0.11.0.tar.gz";
      flake = false;
    };
    entropy = {
      url = "https://hackage.haskell.org/package/entropy-0.4.1.10/entropy-0.4.1.10.tar.gz";
      flake = false;
    };
    hiedb = {
      url = "https://hackage.haskell.org/package/hiedb-0.4.2.0/hiedb-0.4.2.0.tar.gz";
      flake = false;
    };
    hw-prim = {
      url = "https://hackage.haskell.org/package/hw-prim-0.6.3.2/hw-prim-0.6.3.2.tar.gz";
      flake = false;
    };
  };
  outputs =
    inputs@{ self, nixpkgs, flake-compat, flake-utils, gitignore, all-cabal-hashes-unpacked, ... }:
    {
      overlays.default = final: prev:
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
                # Library profiling is disabled as it causes long compilation time
                # on our CI jobs. Nix users are free tor revert this anytime.
                enableLibraryProfiling = false;
                doHaddock = false;
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
            ghcide-bench = ./ghcide-bench;
            hls-graph = ./hls-graph;
            shake-bench = ./shake-bench;
            hie-compat = ./hie-compat;
            hls-plugin-api = ./hls-plugin-api;
            hls-test-utils = ./hls-test-utils;
            ghcide-test-utils = ./ghcide/test;
            # hiedb depends on hie-compact, which is part of this repository. If
            # cabal inside the nix development shell tries to use the hiedb
            # compiled inside nix, it thinks that this package is broken and
            # does nothing. Adding this here ensures that hiedb compiled in nix
            # is not available to cabal and then cabal downloads hiedb from
            # hackage and compiles it.
            hiedb = inputs.hiedb;
          } // pluginSourceDirs;

          # Tweak our packages
          # Don't use `callHackage`, it requires us to override `all-cabal-hashes`
          tweaks = hself: hsuper:
            with haskell.lib; {
              # Patches don't apply
              github = overrideCabal hsuper.github (drv: { patches = []; });
              # GHCIDE requires hie-bios ^>=0.9.1
              hie-bios = hself.callCabal2nix "hie-bios" inputs.hie-bios {};

              lsp = hsuper.callCabal2nix "lsp" inputs.lsp {};
              lsp-types = hsuper.callCabal2nix "lsp-types" inputs.lsp-types {};
              lsp-test = hsuper.callCabal2nix "lsp-test" inputs.lsp-test {};

              entropy = hsuper.callCabal2nix "entropy" inputs.entropy {};
              hiedb = hsuper.callCabal2nix "hiedb" inputs.hiedb {};
              hw-prim = hsuper.callCabal2nix "hw-prim" inputs.hw-prim {};

              implicit-hie-cradle = hself.callCabal2nix "implicit-hie-cradle" inputs.implicit-hie-cradle {};
              ghc-check = hself.callCabal2nix "ghc-check" inputs.ghc-check {};
              # https://github.com/NixOS/nixpkgs/issues/140774
              ormolu =
                if final.system == "aarch64-darwin"
                then overrideCabal hsuper.ormolu (_: { enableSeparateBinOutput = false; })
                else hsuper.ormolu;
            };

          hlsSources =
            builtins.mapAttrs (_: dir: gitignoreSource dir) sourceDirs;

          # Disable tests, but only for the packages mentioned in this overlay
          #
          # We don't want to disable tests for *all* packages
          dontCheck = overlay: hself: hsuper:
            builtins.mapAttrs (_: haskell.lib.dontCheck)
              (overlay hself hsuper);

          extended = hpkgs: hpkgs.override (old: {
            overrides =
              lib.fold
                lib.composeExtensions
                (old.overrides or (_: _: { }))
                [ haskellOverrides
                  (dontCheck (haskell.lib.packageSourceOverrides hlsSources))
                  tweaks
                ];
          });
        in {
          inherit hlsSources;

          all-cabal-hashes = prev.runCommand "all-cabal-hashes.tar.gz"
            { }
            ''
              cd ${all-cabal-hashes-unpacked}
              cd ..
              tar czf $out $(basename ${all-cabal-hashes-unpacked})
            '';

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
          overlays = [ self.overlays.default ];
          config = { allowBroken = true; };
        };

        ghc902Config = (import ./configuration-ghc-90.nix) { inherit pkgs inputs; };
        ghc924Config = (import ./configuration-ghc-92.nix) { inherit pkgs inputs; };
        ghc942Config = (import ./configuration-ghc-94.nix) { inherit pkgs inputs; };

        # GHC versions
        # While HLS still works fine with 8.10 GHCs, we only support the versions that are cached
        # by upstream nixpkgs, which now only includes GHC version 9+
        supportedGHCs = let
          ghcVersion = "ghc" + (pkgs.lib.replaceStrings ["."] [""] pkgs.haskellPackages.ghc.version);
          cases = {
            ghc902 = ghc902Config.tweakHpkgs (pkgs.hlsHpkgs "ghc902");
            ghc924 = ghc924Config.tweakHpkgs (pkgs.hlsHpkgs "ghc924");
            ghc942 = ghc942Config.tweakHpkgs (pkgs.hlsHpkgs "ghc942");
          };
          in { default = cases."${ghcVersion}"; } // cases;

        ghc902 = supportedGHCs.ghc902;
        ghc924 = supportedGHCs.ghc924;
        ghc942 = supportedGHCs.ghc942;
        ghcDefault = supportedGHCs.default;

        pythonWithPackages = pkgs.python3.withPackages (ps: [ps.sphinx ps.myst-parser ps.sphinx_rtd_theme ps.pip]);

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
            pkgs.haskellPackages.hie-bios
            # Dependencies needed to build some parts of hackage
            gmp zlib ncurses
            # Changelog tooling
            (gen-hls-changelogs pkgs.haskellPackages)
            # For the documentation
            pythonWithPackages
            # @guibou: I'm not sure this is needed.
            hlint
            pkgs.haskellPackages.opentelemetry-extra
            capstone tracy
            # ormolu
            # stylish-haskell
            pre-commit
            ] ++ lib.optionals stdenv.isDarwin
              (with darwin.apple_sdk.frameworks; [
                Cocoa
                CoreServices
              ]);

          shellHook = ''
            # @guibou: I'm not sure theses lines are needed
            export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib:${capstone}/lib
            export DYLD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib:${capstone}/lib
            export PATH=$PATH:$HOME/.local/bin

            # Install pre-commit hook
            pre-commit install

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

        mkEnvShell = hpkgs:
          pkgs.lib.mapAttrs (name: value: hpkgs.${name}.env) pkgs.hlsSources;

        # Create a hls executable
        # Copied from https://github.com/NixOS/nixpkgs/blob/210784b7c8f3d926b7db73bdad085f4dc5d79428/pkgs/development/tools/haskell/haskell-language-server/withWrapper.nix#L16
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
          haskell-language-server-902-dev = mkDevShell ghc902 "cabal.project";
          haskell-language-server-924-dev = mkDevShell ghc924 "cabal.project";
          haskell-language-server-942-dev = mkDevShell ghc942 "cabal.project";
        };

        # Developement shell, haskell packages are also provided by nix
        nixDevShells = {
          haskell-language-server-dev-nix = mkDevShellWithNixDeps ghcDefault "cabal.project";
          haskell-language-server-902-dev-nix = mkDevShellWithNixDeps ghc902 "cabal.project";
          haskell-language-server-924-dev-nix = mkDevShellWithNixDeps ghc924 "cabal.project";
          haskell-language-server-942-dev-nix = mkDevShellWithNixDeps ghc942 "cabal.project";
        };

        # The default shell provided by Nixpkgs for a Haskell package (i.e. the
        # one that comes in the `.env` attribute)
        envShells = {
          haskell-language-server-dev-env = mkEnvShell ghcDefault;
          haskell-language-server-902-dev-env = mkEnvShell ghc902;
          haskell-language-server-924-dev-env = mkEnvShell ghc924;
          haskell-language-server-942-dev-env = mkEnvShell ghc942;
        };

        allPackages = {
          haskell-language-server = mkExe ghcDefault;
          haskell-language-server-902 = mkExe ghc902;
          haskell-language-server-924 = mkExe ghc924;
          haskell-language-server-942 = mkExe ghc942;
        };

        devShells = simpleDevShells // nixDevShells // envShells // {
          default = simpleDevShells.haskell-language-server-dev;
        };

        packages = allPackages // {
          default = allPackages.haskell-language-server;

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

        # The attributes for the default shell and package changed in recent versions of Nix,
        # these are here for backwards compatibility with the old versions.
        devShell = devShells.default;
        defaultPackage = packages.default;
      });

  nixConfig = {
    extra-substituters = [
      "https://haskell-language-server.cachix.org"
    ];
    extra-trusted-public-keys = [
      "haskell-language-server.cachix.org-1:juFfHrwkOxqIOZShtC4YC1uT1bBcq2RSvC7OMKx0Nz8="
    ];
  };
}
