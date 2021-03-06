self: super: let
  ytdl-dir = "\${YTDL_DIR:-\${XDG_VIDEOS_DIR:-\${HOME}/Videos}}";
  title-ext-fmt = "%(title)s.%(ext)s";
  video-title-fmt = "%(upload_date)s - ${title-ext-fmt}";
  playlist-index-fmt = "%(playlist_uploader)s/%(playlist)s/%(playlist_index)s";
  playlist-video-fmt = "${playlist-index-fmt}${video-title-fmt}";
  playlist-audio-fmt = "${playlist-index-fmt} - ${title-ext-fmt}";
  youtube-video-fmt = "${ytdl-dir}/%(uploader)s/${video-title-fmt}";
in {
  ytdla = super.writeShellScriptBin "ytdla" ''
    exec "${self.youtube-dl}/bin/youtube-dl" --format "bestaudio/best" --extract-audio "$@"
  '';
  ytdlp = super.writeShellScriptBin "ytdlp" ''
    exec "${self.youtube-dl}/bin/youtube-dl" --output "${playlist-video-fmt}" --yes-playlist "$@"
  '';
  ytdlpa = super.writeShellScriptBin "ytdlpa" ''
    exec "${self.ytdla}/bin/ytdla" --output "${playlist-video-fmt}" --yes-playlist "$@"
  '';
  ytdly = super.writeShellScriptBin "ytdly" ''
    exec "${self.youtube-dl}/bin/youtube-dl" --output "${youtube-video-fmt}" "$@"
  '';
  ytdlay = super.writeShellScriptBin "ytdlay" ''
    exec "${self.ytdla}/bin/ytdla" --output "${youtube-video-fmt}" "$@"
  '';
  ytdlpy = super.writeShellScriptBin "ytdlpy" ''
    exec "${self.youtube-dl}/bin/youtube-dl" --output "${ytdl-dir}/${playlist-video-fmt}" --yes-playlist "$@"
  '';
  ytdlpay = super.writeShellScriptBin "ytdlpay" ''
    exec "${self.ytdla}/bin/ytdla" --output "${ytdl-dir}/${playlist-audio-fmt}" --yes-playlist "$@"
  '';
  ytdl = super.symlinkJoin {
    name = "ytdl";
    paths = with self; [ ytdla ytdlp ytdlpa ytdly ytdlay ytdlpy ytdlpay ];
    postBuild = ''
      ln -s "${self.youtube-dl}/bin/youtube-dl" "$out/bin/ytdl"
    '';
  };
}
