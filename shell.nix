# This file is the compt layer of flakes: https://github.com/edolstra/flake-compat
# See flake.nix for details
(import (
  let
    lock = builtins.fromJSON (builtins.readFile ./flake.lock);
  in fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
    sha256 = lock.nodes.flake-compat.locked.narHash; }
) {
  src =  ./.;
}).shellNix
