self: super: {
  eldev = super.stdenv.mkDerivation rec {
    pname = "eldev";
    version = "0.8.1";
    src = super.fetchurl {
      url = "https://raw.githubusercontent.com/doublep/eldev/${version}/bin/eldev";
      sha256 = "0csn6c4w95iswqdlj5akzspcm5ar7imngqcdch87ac21wz8xigln";
    };
    nativeBuildInputs = [ self.makeWrapper ];
    unpackPhase = "true";
    installPhase = ''
      install -D -v -m555 "$src" "$out/bin/eldev"
      wrapProgram "$out/bin/eldev" --set ELDEV_EMACS "${self.emacs}/bin/emacs"
    '';
  };
}
