self: super:
let unstable-channel = import (builtins.fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz) {};
in {
  zoomUs = unstable-channel.zoom-us;
}
