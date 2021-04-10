# This shell.nix file is designed for use with cabal build
# It does **not** aim to replace Cabal

# Maintaining this file:
#
#     - Bump the nixpkgs version using `niv update nixpkgs`
#     - Edit `ourSources` (in nix/default.nix) to update the set of local packages
#
# For more details: https://github.com/NixOS/nixpkgs/blob/20.03/pkgs/development/haskell-modules/make-package-set.nix#L256


{ compiler ? "default",
  withHoogle ? false,
  nixpkgs ? import ./nix {}
 }:

with nixpkgs;

let defaultCompiler = "ghc" + lib.replaceStrings ["."] [""] haskellPackages.ghc.version;
    haskellPackagesForProject =
        if compiler == "default"
            then ourHaskell.packages.${defaultCompiler}
            else ourHaskell.packages.${compiler};

    packages = p: with builtins; map (name: p.${name}) (attrNames ourSources);

    isSupported = compiler == "default" || compiler == defaultCompiler;
in
haskellPackagesForProject.shellFor {
  inherit withHoogle;
  doBenchmark = true;
  packages = p: if isSupported then packages p else [p.ghc-paths];
  buildInputs = [
    gmp
    zlib
    ncurses
    capstone
    tracy

    genChangelogs

    haskellPackages.cabal-install
    haskellPackages.hlint
    haskellPackages.ormolu
    haskellPackages.stylish-haskell
    haskellPackages.opentelemetry-extra

  ];
  src = null;
  shellHook = ''
    export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib:${capstone}/lib
    export DYLD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib:${capstone}/lib
    export PATH=$PATH:$HOME/.local/bin
    ${pre-commit-check.shellHook}
  '';
}
