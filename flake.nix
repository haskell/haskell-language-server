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

    # List of hackage dependencies
    ghc-lib-parser-94 = {
      url = "https://hackage.haskell.org/package/ghc-lib-parser-9.4.4.20221225/ghc-lib-parser-9.4.4.20221225.tar.gz";
      flake = false;
    };
    hlint-35 = {
      url = "https://hackage.haskell.org/package/hlint-3.5/hlint-3.5.tar.gz";
      flake = false;
    };
    ptr-poker = {
      url = "https://hackage.haskell.org/package/ptr-poker-0.1.2.8/ptr-poker-0.1.2.8.tar.gz";
      flake = false;
    };
    ormolu-052 = {
      url = "https://hackage.haskell.org/package/ormolu-0.5.2.0/ormolu-0.5.2.0.tar.gz";
      flake = false;
    };
    stylish-haskell = {
      url = "https://hackage.haskell.org/package/stylish-haskell-0.14.4.0/stylish-haskell-0.14.4.0.tar.gz";
      flake = false;
    };
  };
  outputs =
    inputs@{ self, nixpkgs, flake-compat, flake-utils, gitignore, ... }:
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
          } // pluginSourceDirs;

          # Tweak our packages
          # Don't use `callHackage`, it requires us to override `all-cabal-hashes`
          tweaks = hself: hsuper:
            with haskell.lib; {
              # Patches don't apply
              github = overrideCabal hsuper.github (drv: { patches = []; });

              # https://github.com/NixOS/nixpkgs/issues/140774
              ormolu =
                if final.system == "aarch64-darwin"
                then overrideCabal hsuper.ormolu (_: { enableSeparateBinOutput = false; })
                else hsuper.ormolu;

              # Due to the following issue, fixity-th should be disabled, especially for darwin.
              # https://github.com/fourmolu/fourmolu/issues/238
              # nixpkgs now disables fixity-th for ghc944.
              fourmolu =
                addBuildDepend
                  (appendConfigureFlag hself.fourmolu_0_10_1_0 "-f-fixity-th")
                  hself.file-embed;

              stylish-haskell = hself.callCabal2nix "stylish-haskell" inputs.stylish-haskell {};
            };

          hlsSources =
            builtins.mapAttrs (_: dir: gitignoreSource dir) sourceDirs;

          # Disable tests, but only for the packages mentioned in this overlay
          #
          # We don't want to disable tests for *all* packages
          dontCheck = overlay: hself: hsuper:
            builtins.mapAttrs (_: haskell.lib.dontCheck)
              (overlay hself hsuper);

          applyHaskellOverlays = overlays: hpkgs: hpkgs.override (old: {
            overrides =
              lib.fold
                lib.composeExtensions
                (old.overrides or (_: _: { }))
                overlays;
          });

          extended = forHlsCI:
            applyHaskellOverlays
              (prev.lib.optional forHlsCI haskellOverrides
               ++ [ (dontCheck (haskell.lib.packageSourceOverrides hlsSources))
                    tweaks
                  ]
              );
        in {
          inherit hlsSources;

          # Haskell packages extended with our packages
          hlsHpkgs = compiler: extended true haskell.packages.${compiler};
          # Haskell packages extended with our packages; reusing the nixpkgs set as much as possible
          hlsHpkgsNixpkgs = compiler: extended false haskell.packages.${compiler};

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
        ghc927Config = (import ./configuration-ghc-92.nix) { inherit pkgs inputs; };
        ghc944Config = (import ./configuration-ghc-94.nix) { inherit pkgs inputs; };

        # GHC versions
        # While HLS still works fine with 8.10 GHCs, we only support the versions that are cached
        # by upstream nixpkgs, which now only includes GHC version 9+
        supportedGHCs = let
          ghcVersion = "ghc" + (pkgs.lib.replaceStrings ["."] [""] pkgs.haskellPackages.ghc.version);
          cases = {
            ghc902 = ghc902Config.tweakHpkgs (pkgs.hlsHpkgs "ghc902");
            ghc927 = ghc927Config.tweakHpkgs (pkgs.hlsHpkgs "ghc927");
            ghc944 = ghc944Config.tweakHpkgs (pkgs.hlsHpkgs "ghc944");
          };
          in { default = cases."${ghcVersion}"; } // cases;

        ghc902 = supportedGHCs.ghc902;
        ghc927 = supportedGHCs.ghc927;
        ghc944 = supportedGHCs.ghc944;
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
            (pkgs.haskell.lib.justStaticExecutables (pkgs.haskell.lib.dontCheck pkgs.haskellPackages.opentelemetry-extra))
            capstone
            # ormolu
            # stylish-haskell
            pre-commit
            ] ++ lib.optionals (!stdenv.isDarwin)
                   [ # tracy has a build problem on macos.
                     tracy
                   ]
              ++ lib.optionals stdenv.isDarwin
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
          haskell-language-server-927-dev = mkDevShell ghc927 "cabal.project";
          haskell-language-server-944-dev = mkDevShell ghc944 "cabal.project";
        };

        # Developement shell, haskell packages are also provided by nix
        nixDevShells = {
          haskell-language-server-dev-nix = mkDevShellWithNixDeps ghcDefault "cabal.project";
          haskell-language-server-902-dev-nix = mkDevShellWithNixDeps ghc902 "cabal.project";
          haskell-language-server-927-dev-nix = mkDevShellWithNixDeps ghc927 "cabal.project";
          haskell-language-server-944-dev-nix = mkDevShellWithNixDeps ghc944 "cabal.project";
        };

        # The default shell provided by Nixpkgs for a Haskell package (i.e. the
        # one that comes in the `.env` attribute)
        envShells = {
          haskell-language-server-dev-env = mkEnvShell ghcDefault;
          haskell-language-server-902-dev-env = mkEnvShell ghc902;
          haskell-language-server-927-dev-env = mkEnvShell ghc927;
          haskell-language-server-944-dev-env = mkEnvShell ghc944;
        };

        allPackages = {
          haskell-language-server = mkExe ghcDefault;
          haskell-language-server-902 = mkExe ghc902;
          haskell-language-server-927 = mkExe ghc927;
          haskell-language-server-944 = mkExe ghc944;
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
          all-nix-dev-shells = linkFarmFromDrvs "all-dev-shells"
            (builtins.map (shell: shell.inputDerivation) (lib.unique (builtins.attrValues nixDevShells)));

          all-simple-dev-shells = linkFarmFromDrvs "all-simple-dev-shells"
            (builtins.map (shell: shell.inputDerivation) (lib.unique (builtins.attrValues simpleDevShells)));

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
