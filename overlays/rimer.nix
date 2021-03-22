self: super: {
  rimer = import (super.fetchurl {
    url = "https://raw.githubusercontent.com/xFA25E/rimer/f7a4f68389fd0055c45490c4a3d3b0a05a6e668a/default.nix";
    sha256 = "1bgkpvrx3nhds3sh462xghkqpkfwbcpx50s0kagql9gh0m62api2";
  }) { pkgs = self; };
}
