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

  ytdl-dir = "\\\${YTDL_DIR:-\\\${XDG_VIDEOS_DIR:-\\\${HOME}/Videos}/youtube}";
  title-ext-fmt = "%(title)s.%(ext)s";
  video-title-fmt = "%(upload_date)s - ${title-ext-fmt}";
  playlist-index-fmt = "%(playlist_uploader)s - %(playlist)s - %(playlist_index)s";
  playlist-video-fmt = "${playlist-index-fmt}${video-title-fmt}";
  playlist-audio-fmt = "${playlist-index-fmt} - ${title-ext-fmt}";
  youtube-video-fmt = "${ytdl-dir}/%(channel)s - ${video-title-fmt}";

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
        "mpvi" = [ mpv libnotify jq gnused unixtools.column dmenu "$out" ];
        "notify_sound" = [ mpv ];
        "qrshow" = [ libnotify coreutils qrencode sxiv ];
        "search_ebook" = [ coreutils findutils ];
        "strip_video" = [ ffmpeg ];
        "studies_plot" = [ gnuplot ];
        "sudo_askpass" = [ pass ];
        "video_duration" = [ ffmpeg jq ];
        "ytdlam" = [ findutils coreutils dmenu "$out" ];
        "ytdli" = [ libnotify jq gnused unixtools.column dmenu pueue bash gawk "$out" ];
      };
      wrapPrefixScripts = with self; { # sudo required
        "rmount" = [ coreutils utillinux gawk findutils dmenu mtpfs ];
        "rumount" = [ dmenu gawk utillinux libnotify gnugrep ];
      };
      wrapFlags = from: to: "makeWrapper \"$out/bin/${from}\" \"$out/bin/${to}\" --add-flags";
    in ''
      ${wrapPaths wrapScripts}
      ${wrapPrefixPaths wrapPrefixScripts}
      ln -s "${self.youtube-dl}/bin/youtube-dl" "$out/bin/ytdl"
      ${wrapFlags "ytdl" "ytdla"} "--format bestaudio/best --extract-audio"
      ${wrapFlags "ytdl" "ytdlp"} "--output \"${playlist-video-fmt}\" --yes-playlist"
      ${wrapFlags "ytdla" "ytdlpa"} "--output \"${playlist-video-fmt}\" --yes-playlist"
      ${wrapFlags "ytdl" "ytdly"} "--output \"${youtube-video-fmt}\" --exec \"$out/bin/filename_put_duration {}\""
      ${wrapFlags "ytdla" "ytdlay"} "--output \"${youtube-video-fmt}\" --exec \"$out/bin/filename_put_duration {}\""
      ${wrapFlags "ytdl" "ytdlpy"} "--output \"${ytdl-dir}/${playlist-video-fmt}\" --yes-playlist"
      ${wrapFlags "ytdla" "ytdlpay"} "--output \"${ytdl-dir}/${playlist-audio-fmt}\" --yes-playlist"
    '';
  };
}
