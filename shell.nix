# This shell.nix file is designed for use with cabal build
# It aims to leverage the nix cache in as much as possible
# while reducing Nix maintenance costs.
# It does **not** aim to replace Cabal/Stack with Nix


{ sources ? import nix/sources.nix,
  nixpkgs ? import sources.nixpkgs {},
  compiler ? "default"
 }:
with nixpkgs;

let haskellPackagesForProject = p:
        if compiler == "default" || compiler == "ghc883"
            then haskellPackages.ghcWithPackages p
            # for all other compilers there is no Nix cache so dont bother building deps with NIx
            else haskell.packages.${compiler}.ghcWithPackages [];

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
    gmp
    zlib
    ncurses

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
