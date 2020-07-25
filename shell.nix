{ sources ? import nix/sources.nix,
  nixpkgs ? import sources.nixpkgs {},
  compiler ? "default"
 }:
with nixpkgs;

let haskellPackagesForProject = if compiler == "default"
    then haskellPackages.ghcWithPackages
    else haskell.packages.${compiler}.ghcWithPackages;

   compilerWithPackages = haskellPackagesForProject(p:
        with p;
        [ aeson
          async
          extra
          gitrev
          haskell-lsp
          lens
          network
          optparse-simple
          prettyprinter
          QuickCheck
          regex-tdfa
          rope-utf16-splay
          safe-exceptions
          shake
          tasty
          tasty-golden
          tasty-hunit
          tasty-rerun
          temporary
          text
          unordered-containers
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
