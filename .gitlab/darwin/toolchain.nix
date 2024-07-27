{ system }:

let
  sources = import ./nix/sources.nix;
  nixpkgsSrc = sources.nixpkgs;
  pkgs = import nixpkgsSrc { inherit system; };
in

let
  hsPkgs = pkgs.haskellPackages;
  alex = hsPkgs.alex;
  happy = hsPkgs.happy;
  targetTriple = pkgs.stdenv.targetPlatform.config;

  llvm = pkgs.llvm_11;
in
pkgs.writeTextFile {
  name = "toolchain";
  text = ''
    export PATH
    PATH="${pkgs.autoconf}/bin:$PATH"
    PATH="${pkgs.automake}/bin:$PATH"
    PATH="${pkgs.tree}/bin:$PATH"
    export HAPPY="${happy}/bin/happy"
    export ALEX="${alex}/bin/alex"
    export LLC="${llvm}/bin/llc"
    export OPT="${llvm}/bin/opt"
    export SPHINXBUILD="${pkgs.python3Packages.sphinx}/bin/sphinx-build"
    export CABAL_INSTALL="${pkgs.cabal-install}/bin/cabal"
    export CABAL="$CABAL_INSTALL"

    sdk_path="$(xcrun --sdk macosx --show-sdk-path)"
  '';
}
