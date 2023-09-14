# Maintaining this file:
#
#     - Bump the inputs version using `nix flake update`
#     - Edit `sourceDirs` to update the set of local packages
#
# For more details: https://nixos.wiki/wiki/Flakes
{
  description = "haskell language server flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/haskell-updates";
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
    hlint-35 = {
      url = "https://hackage.haskell.org/package/hlint-3.5/hlint-3.5.tar.gz";
      flake = false;
    };
    hlint-36 = {
      url = "https://hackage.haskell.org/package/hlint-3.6.1/hlint-3.6.1.tar.gz";
      flake = false;
    };
    fourmolu-011 = {
      url = "https://hackage.haskell.org/package/fourmolu-0.11.0.0/fourmolu-0.11.0.0.tar.gz";
      flake = false;
    };
    fourmolu-012 = {
      url = "https://hackage.haskell.org/package/fourmolu-0.12.0.0/fourmolu-0.12.0.0.tar.gz";
      flake = false;
    };
    ormolu-052 = {
      url = "https://hackage.haskell.org/package/ormolu-0.5.2.0/ormolu-0.5.2.0.tar.gz";
      flake = false;
    };
    ormolu-07 = {
      url = "https://hackage.haskell.org/package/ormolu-0.7.1.0/ormolu-0.7.1.0.tar.gz";
      flake = false;
    };
    stylish-haskell-0145 = {
      url = "https://hackage.haskell.org/package/stylish-haskell-0.14.5.0/stylish-haskell-0.14.5.0.tar.gz";
      flake = false;
    };

    # not sure if this is the correct way to get lsp* packages in
    lsp = {
      url = "https://hackage.haskell.org/package/lsp-2.2.0.0/lsp-2.2.0.0.tar.gz";
      flake = false;
    };
    lsp-types = {
      url = "https://hackage.haskell.org/package/lsp-types-2.0.2.0/lsp-types-2.0.2.0.tar.gz";
      flake = false;
    };
    lsp-test = {
      url = "https://hackage.haskell.org/package/lsp-test-0.16.0.0/lsp-test-0.16.0.0.tar.gz";
      flake = false;
    };

    haskell-hie-bios = {
      url = "github:haskell/hie-bios";
      flake = false;
    };
    # smunix: github:haskell/hie-bios defines
    #   'CabalType :: Maybe String -> Maybe FilePath -> CabalType'
    # while the original githcom:Avi-D-coder/hie-bios still has this:
    #   'CabalType :: Maybe String -> CabalType'
    # We need a patched version of implicit-hie-cradle that works with hls, so I've created
    # the repository below. Obviously, this is not sustainable as it adds more technical debt.
    # We need a better strategy to streamline changes required by HLS from other hie-bios related
    # packages.
    # See details here: https://github.com/Avi-D-coder/implicit-hie-cradle/compare/master...smunix:implicit-hie-cradle:smunix-patch-hls-1?expand=1
    #
    haskell-implicit-hie-cradle = {
      url = "github:smunix/implicit-hie-cradle?ref=smunix-patch-hls-0.5-1";
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
    } // (flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" "aarch64-linux" "aarch64-darwin" ])
    (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ self.overlays.default ];
          config = { allowBroken = true; };
        };

        ghc90Config = (import ./configuration-ghc-90.nix) { inherit pkgs inputs; };
        ghc92Config = (import ./configuration-ghc-92.nix) { inherit pkgs inputs; };
        ghc94Config = (import ./configuration-ghc-94.nix) { inherit pkgs inputs; };
        ghc96Config = (import ./configuration-ghc-96.nix) { inherit pkgs inputs; };

        # GHC versions
        # While HLS still works fine with 8.10 GHCs, we only support the versions that are cached
        # by upstream nixpkgs, which now only includes GHC version 9+
        supportedGHCs = let
          ghcVersion = "ghc" + (builtins.concatStringsSep "" (pkgs.lib.lists.init (builtins.splitVersion pkgs.haskellPackages.ghc.version)));
          cases = {
            ghc90 = ghc90Config.tweakHpkgs (pkgs.hlsHpkgs "ghc90");
            ghc92 = ghc92Config.tweakHpkgs (pkgs.hlsHpkgs "ghc92");
            ghc94 = ghc94Config.tweakHpkgs (pkgs.hlsHpkgs "ghc94");
            ghc96 = ghc96Config.tweakHpkgs (pkgs.hlsHpkgs "ghc96");
          };
        in { default = cases."${ghcVersion}"; } // cases;

        ghc90 = supportedGHCs.ghc90;
        ghc92 = supportedGHCs.ghc92;
        ghc94 = supportedGHCs.ghc94;
        ghc96 = supportedGHCs.ghc96;
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
          # version in scope (the default one, an
          # The advantage is that we won't have to rebuild theses tools (and
          # dependencies) with a recent GHC which may not be supported by
          # them.
          buildInputs = [
            # our compiling toolchain
            hpkgs.ghc
            hpkgs.cabal-install
            # @guibou: I'm not sure hie-bios is needed
            # pkgs.haskellPackages.hie-bios
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
          haskell-language-server-90-dev = mkDevShell ghc90 "cabal.project";
          haskell-language-server-92-dev = mkDevShell ghc92 "cabal.project";
          haskell-language-server-94-dev = mkDevShell ghc94 "cabal.project";
          haskell-language-server-96-dev = mkDevShell ghc96 "cabal.project";
        };

        # Developement shell, haskell packages are also provided by nix
        nixDevShells = {
          haskell-language-server-dev-nix = mkDevShellWithNixDeps ghcDefault "cabal.project";
          haskell-language-server-90-dev-nix = mkDevShellWithNixDeps ghc90 "cabal.project";
          haskell-language-server-92-dev-nix = mkDevShellWithNixDeps ghc92 "cabal.project";
          haskell-language-server-94-dev-nix = mkDevShellWithNixDeps ghc94 "cabal.project";
          haskell-language-server-96-dev-nix = mkDevShellWithNixDeps ghc96 "cabal.project";
        };

        # The default shell provided by Nixpkgs for a Haskell package (i.e. the
        # one that comes in the `.env` attribute)
        envShells = {
          haskell-language-server-dev-env = mkEnvShell ghcDefault;
          haskell-language-server-90-dev-env = mkEnvShell ghc90;
          haskell-language-server-92-dev-env = mkEnvShell ghc92;
          haskell-language-server-94-dev-env = mkEnvShell ghc94;
          haskell-language-server-96-dev-env = mkEnvShell ghc96;
        };

        allPackages = {
          haskell-language-server = mkExe ghcDefault;
          haskell-language-server-90 = mkExe ghc90;
          haskell-language-server-92 = mkExe ghc92;
          haskell-language-server-94 = mkExe ghc94;
          haskell-language-server-96 = mkExe ghc96;
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
