# This file is an alternative to shell.nix that uses haskell.nix
# instead of the haskell package set from nixpkgs.
#
# To run a shell use the shell from this file run:
#
#    nix-shell haskell.nix -A shell
#
# Significant differences to shell.nix:
#
# * It uses the `nixpkgs-unstable` pin of nixpkgs from haskell.nix
#   instead of the niv pin (this will increase the chances of getting
#   a cache hit when using the haskell.nix binary cache).
#
# * Haskell.nix reads the `source-repository-package` config from
#   the `cabal.project` and other info from `plan.json`.  This means
#   it does not depend on `nix/default.nix` and there is no list
#   of the projects packages in this file to update.
#
# Maintaining this file
#
# * Bump the haskell.nix version using `niv update haskell.nix`.
#   This will update the hackage snapshot used to build the
#   dependencies of the project and any `tools` set to `latest` below
#   will be updated to the latest version in that snapshot.
#
{ compiler-nix-name ? "ghc8104"
, withHoogle ? false
} :
let
  sources = import nix/sources.nix;
  haskellNix = import sources."haskell.nix" {};
  pkgs = import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs;
  project = pkgs.haskell-nix.cabalProject' {
    inherit compiler-nix-name;
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "haskell-language-server";
      src = ./.;
    };
  };
in project // {
  shell = project.shellFor {
    inherit withHoogle;
    packageSetupDeps = false;
    buildInputs = with pkgs; [
      # Add tools from nixpkgs here
    ] ++ builtins.attrValues (
      # Because the exe name is not the pacakge name
      # we can't put `opentelemetry-extra` in the `tools`
      pkgs.haskell-nix.hackage-package {
        inherit compiler-nix-name;
        name = "opentelemetry-extra";
        version = "latest";
      }).components.exes;
    tools = {
      # Tools from hackage (replace `latest` with a version number to pin)
      cabal = "latest";
      hlint = "latest";
      ormolu = "latest";
      stylish-haskell = "latest";
    };
  };
}
