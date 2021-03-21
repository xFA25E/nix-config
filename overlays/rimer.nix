self: super: {
  rimer = import (super.fetchurl {
    url = "https://raw.githubusercontent.com/xFA25E/rimer/eb984f1b5a313e9c1ca46e995403d747eb50c2bb/default.nix";
    sha256 = "1rxihdnbpmkg4zvs2s1l20sv1n4r9xqs50yfayqd26s1s060n5z0";
  }) { pkgs = self; };
}
