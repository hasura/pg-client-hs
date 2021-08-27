{ system ? builtins.currentSystem }:

# let
#   lockfile = builtins.fromJSON (builtins.readFile ../flake.lock);
#   flake-compat =
#     let
#       inherit (lockfile.nodes.flake-compat.locked) narHash rev;
#     in
#     import (builtins.fetchTarball {
#       url = "https://github.com/edolstra/flake-compat/archive/${rev}.tar.gz";
#       sha256 = narHash;
#     });

# in
# import flake-compat {
#   inherit system;
#   src = ../.;
# }

let
  lockfile = builtins.fromJSON (builtins.readFile ../flake.lock);
  inherit (lockfile.nodes.flake-compat.locked) narHash rev;
  flake-compat = fetchTarball {
    url = "https://github.com/edolstra/flake-compat/archive/${rev}.tar.gz";
    sha256 = narHash;
  };
in
import flake-compat {
  inherit system;
  src = ../.;
}
