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
  # TODO Remove allowBroken once retrie is no longer marked as broken in Nixpkgs
  # See https://github.com/haskell/haskell-language-server/issues/325
  nixpkgs ? import sources.nixpkgs { config.allowBroken = true; },
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

    retrie = with haskell.lib; dontCheck(disableLibraryProfiling(haskellPackages.retrie));
    compilerWithPackages = haskellPackagesForProject(p:
        with p;
        [
          Diff
          Glob
          HsYAML-aeson
          QuickCheck
          aeson
          alex
          async
          base16-bytestring
          blaze-builder
          blaze-markup
          brittany
          conduit-extra
          conduit-parse
          cryptohash-sha1
          data-default
          data-default-class
          data-default-instances-containers
          data-default-instances-dlist
          data-default-instances-old-locale
          extra
          floskell
        #   fourmolu
          fuzzy
          generic-deriving
          ghc-check
          gitrev
          haddock-library
          happy
          haskell-lsp
          haskell-src-exts
          hie-bios
          hslogger
          hspec
          lens
          lsp-test
          megaparsec
          network
          opentelemetry
          optparse-simple
          ormolu
          parser-combinators
          parsers
          prettyprinter
          prettyprinter-ansi-terminal
          primes
          psqueues
          regex-tdfa
          retrie
          rope-utf16-splay
          safe-exceptions
          shake
          sorted-list
          strict
          stylish-haskell
          tasty
          tasty-ant-xml
          tasty-expected-failure
          tasty-golden
          tasty-hunit
          tasty-rerun
          temporary
          text
          typed-process
          unix-compat
          unordered-containers
          xml
          yaml
          zlib
         ]);
in
stdenv.mkDerivation {
  name = "haskell-language-server";
  buildInputs = [
    git
    gmp
    ncurses
    zlib

    haskellPackages.cabal-install
    haskellPackages.hlint

    compilerWithPackages

  ];
  src = null;
  shellHook = ''
    export LD_LIBRARY_PATH=${gmp}/lib:${zlib}/lib:${ncurses}/lib
    export PATH=$PATH:$HOME/.local/bin
  '';
}
