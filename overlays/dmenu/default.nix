self: super: {
  # dmenu = super.dmenu.override { patches = [ ./dmenu.patch ]; };
  dmenu = super.symlinkJoin {
    name = "rofi-with-dmenu";
    paths = [ super.rofi ];
    nativeBuildInputs = [ super.makeWrapper ];
    postBuild = ''
      makeWrapper $out/bin/rofi $out/bin/dmenu --add-flags -dmenu
    '';
  };
}
