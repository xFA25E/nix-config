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
    src = ./.;
    dontUnpack = true;
    dontPatch = true;
    dontConfigure = true;
    dontBuild = true;
    nativeBuildInputs = [ super.makeWrapper ];
    installPhase = ''
      install -D -t "$out/bin" "$src/"*
      rm "$out/bin/default.nix"
    '';
    postFixup = let
      wrapScripts = with self; {
        "compress_video" = [ ffmpeg ];
        "extract_eml" = [ coreutils mu ];
        "filename_put_duration" = [ coreutils "$out" ];
        "format_duration" = [ gawk ];
        "image_clipboard" = [ file xclip ];
        "image-dired-external-viewer" = [ mpv sxiv ];
        "make_backup" = [ utillinux coreutils rsync gawk dmenu ];
        "mpvi" = [ mpv libnotify jq gnused ytdl unixtools.column dmenu ];
        "notify_sound" = [ mpv ];
        "qrshow" = [ libnotify coreutils qrencode sxiv ];
        "search_ebook" = [ coreutils findutils ];
        "strip_video" = [ ffmpeg ];
        "studies_plot" = [ gnuplot ];
        "sudo_askpass" = [ pass ];
        "video_duration" = [ ffmpeg jq ];
        "ytdlam" = [ findutils coreutils dmenu ytdla ];
        "ytdli" = [ libnotify jq gnused unixtools.column dmenu pueue bash ytdly ytdlay "$out" gawk ];
      };
      wrapPrefixScripts = with self; { # sudo required
        "rmount" = [ coreutils utillinux gawk findutils dmenu mtpfs ];
        "rumount" = [ dmenu gawk utillinux libnotify gnugrep ];
      };
    in ''
      ${wrapPaths wrapScripts}
      ${wrapPrefixPaths wrapPrefixScripts}
    '';
  };
}
