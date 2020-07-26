{ sources ? import nix/sources.nix,
  nixpkgs ? import sources.nixpkgs {},
  compiler ? "default"
 }:
with nixpkgs;

let haskellPackagesForProject = if compiler == "default"
    then haskellPackages.ghcWithPackages
    else haskell.packages.${compiler}.ghcWithPackages;

    # these packages fail to build with ghc8101
    extraPackages = p: if compiler == "ghc8101"
      then []
      else [p.haskell-lsp p.lsp-test];

   compilerWithPackages = haskellPackagesForProject(p:
        with p;
        [ aeson
          alex
          async
          base16-bytestring
          blaze-builder
          blaze-markup
          conduit-extra
          conduit-parse
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
          gitrev
          Glob
          happy
          haskell-src-exts
          hslogger
          hspec
          lens
          network
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
          tasty
          tasty-golden
          tasty-hunit
          tasty-rerun
          temporary
          text
          typed-process
          unordered-containers
          xml
          yaml
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
