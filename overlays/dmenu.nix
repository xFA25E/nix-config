self: super: let
  fn = "monospace:size=13";
  bg = "#FFFFFF";
  fg = "#000000";
in {
  dmenu = super.symlinkJoin {
    name = "dmenu";
    paths = [ super.dmenu ];
    nativeBuildInputs = [ super.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/dmenu --add-flags '-i -l 10 -fn "${fn}" -nf "${fg}" -nb "${bg}" -sf "${bg}" -sb "${fg}"'
    '';
  };
}
