self: super: {
  eldev = super.stdenv.mkDerivation rec {
    pname = "eldev";
    version = "0.9.2";
    src = super.fetchurl {
      url = "https://raw.githubusercontent.com/doublep/eldev/${version}/bin/eldev";
      sha256 = "0ikhhfxm1rz3wp37spsy8bcnx5071ard71pd1riw09rsybilxhgn";
    };
    nativeBuildInputs = [ super.makeWrapper ];
    unpackPhase = "true";
    installPhase = ''
      install -D -v -m555 "$src" "$out/bin/eldev"
      wrapProgram "$out/bin/eldev" --set ELDEV_EMACS "${self.emacs}/bin/emacs"
    '';
  };
}
