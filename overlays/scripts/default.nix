self: super: let
  wrapPath = name: paths: ''
    wrapProgram "$out/bin/${name}" --set PATH "${self.lib.strings.makeBinPath paths}"
  '';
  wrapPaths = scripts: super.lib.strings.concatStringsSep "\n"
    (super.lib.attrsets.mapAttrsToList wrapPath scripts);
  wrapPrefixPath = name: paths: ''
    wrapProgram "$out/bin/${name}" --prefix PATH : "${self.lib.strings.makeBinPath paths}"
  '';
  wrapPrefixPaths = scripts: super.lib.strings.concatStringsSep "\n"
    (super.lib.attrsets.mapAttrsToList wrapPrefixPath scripts);
in {
  scripts = super.stdenv.mkDerivation {
    name = "scripts";
    src = ./scripts;
    dontUnpack = true;
    dontPatch = true;
    dontConfigure = true;
    dontBuild = true;
    nativeBuildInputs = [ super.makeWrapper ];
    installPhase = ''
      install -D -t "$out/bin" "$src/"*
    '';
    postFixup = let
      wrapScripts = with self; {
        "compress_video" = [ ffmpeg ];
        "extract_eml" = [ coreutils mu ];
        "format_duration" = [ gawk ];
        "image_clipboard" = [ file xclip ];
        "make_video_queue" = [ findutils coreutils gawk "$out" gnused unixtools.column ];
        "mpvi" = [ mpv libnotify jq gnused ytdl unixtools.column dmenu ];
        "notify_sound" = [ mpv ];
        "qrshow" = [ libnotify coreutils qrencode sxiv ];
        "random_wallpaper" = [ feh coreutils ];
        "rimer_callback" = [ coreutils libnotify "$out" gawk dmenu ];
        "search_ebook" = [ coreutils findutils ];
        "ssh-askpass" = [ pass findutils coreutils gnused dmenu ];
        "strip_video" = [ ffmpeg ];
        "studies_plot" = [ gnuplot ];
        "sudo_askpass" = [ pass ];
        "video_duration" = [ ffmpeg jq ];
        "ytdlam" = [ findutils coreutils dmenu ytdla ];
        "ytdli" = [ libnotify jq gnused unixtools.column dmenu pueue bash ytdly ytdlay "$out" gawk ];
      };
      wrapPrefixScripts = with self; { # sudo required
        "make_backup" = [ util-linux coreutils rsync ];
        "rmount" = [ coreutils util-linux gawk findutils dmenu mtpfs ];
        "rumount" = [ dmenu gawk util-linux libnotify gnugrep ];
      };
    in ''
      ${wrapPaths wrapScripts}
      ${wrapPrefixPaths wrapPrefixScripts}
    '';
  };
}