self: super: {
  udiskie = super.symlinkJoin {
    name = "udiskie";
    paths = [ super.udiskie ];
    nativeBuildInputs = [ super.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/udiskie --prefix PATH : ${self.dbus}/bin
    '';
  };
}
