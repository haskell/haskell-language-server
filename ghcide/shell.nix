# This shell.nix file is designed for use with cabal build
# It aims to leverage the nix cache in as much as possible
# while reducing Nix maintenance costs.
# It does **not** aim to replace Cabal/Stack with Nix

# Maintaining this file:
#
# - Dealing with broken nix-shell
#
#     1. Bump the nixpkgs version using `niv update nixpkgs`
#     2. Comment out any remaining failing packages
#
# - Dealing with broken cabal build inside nix-shell:
#
#    If my understanding of cabal new-build is correct this should never happen,
#    assuming that cabal new-build does succeed outside nix-shell

{ sources ? import nix/sources.nix,
  nixpkgs ? import sources.nixpkgs {},
  compiler ? "default",
  hoogle ? false
 }:
with nixpkgs;

let defaultCompiler = "ghc" + lib.replaceStrings ["."] [""] haskellPackages.ghc.version;
    haskellPackagesForProject = p:
        if compiler == "default" || compiler == defaultCompiler
            then if hoogle
                then haskellPackages.ghcWithHoogle p
                else haskellPackages.ghcWithPackages p
            # for all other compilers there is no Nix cache so dont bother building deps
            else if hoogle
                then  haskell.packages.${compiler}.ghcWithHoogle (_: [])
                else haskell.packages.${compiler}.ghcWithPackages (_: []);

   compilerWithPackages = haskellPackagesForProject(p:
        with p;
        [ aeson
          async
          base16-bytestring
          Chart
          Chart-diagrams
          cryptohash-sha1
          data-default
          diagrams
          diagrams-svg
          extra
          fuzzy
          fingertree
          Glob
          ghc-check
          gitrev
          happy
          haskell-lsp
          haskell-lsp-types
          hie-bios
          hslogger
          lens
          lsp-test
          network
          optparse-applicative
          QuickCheck
          quickcheck-instances
          prettyprinter
          prettyprinter-ansi-terminal
          regex-tdfa
          rope-utf16-splay
          safe
          safe-exceptions
          shake
          sorted-list
          stm
          syb
          tasty
          tasty-expected-failure
          tasty-hunit
          tasty-rerun
          tasty-quickcheck
          temporary
          text
          time
          transformers
          typed-process
          unordered-containers
          utf8-string
          yaml
         ]);
in
stdenv.mkDerivation {
  name = "ghcide";
  buildInputs = [
    gmp
    zlib
    ncurses

    haskellPackages.cabal-install
    haskellPackages.hlint
    haskellPackages.ormolu
    haskellPackages.stylish-haskell

    compilerWithPackages

  ];
  src = null;
  shellHook = ''
    export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib
    export PATH=$PATH:$HOME/.local/bin
  '';
}
