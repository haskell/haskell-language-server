# This shell.nix file is designed for use with cabal build
# It aims to leverage the nix cache in as much as possible
# It does not aim to replace Cabal/Stack with Nix

# Before making changes, ensure that the Nix expression works
# for all the GHC versions supported by the project (8.6.x - 8.10.x)

{ sources ? import nix/sources.nix,
  nixpkgs ? import sources.nixpkgs {},
  compiler ? "default"
 }:
with nixpkgs;

let haskellPackagesForProject = if compiler == "default"
    then haskellPackages.ghcWithPackages
    else haskell.packages.${compiler}.ghcWithPackages;

    extraPackages = p: with p;
    if compiler == "ghc8101" then 
          [conduit-extra
           conduit-parse
           hie-bios
           yaml
          ]
        else if compiler == "ghc865" then [] else
         # compiler = ghc88
          [ haskell-lsp 
            lsp-test
            brittany
            ormolu
            stylish-haskell
           ];

   compilerWithPackages = haskellPackagesForProject(p:
        with p;
        [ aeson
          alex
          async
          base16-bytestring
          blaze-builder
          blaze-markup
          cryptohash-sha1
          data-default
          data-default-class
          data-default-instances-containers
          data-default-instances-dlist
          data-default-instances-old-locale
          Diff
          extra
          floskell
          fuzzy
          generic-deriving
          ghc-check
          gitrev
          Glob
          haddock-library
          happy
          haskell-src-exts
          hslogger
          hspec
          HsYAML-aeson
          lens
          megaparsec
          network
          opentelemetry
          optparse-simple
          QuickCheck
          parsers
          parser-combinators
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
          zlib
         ] ++ extraPackages p);
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
