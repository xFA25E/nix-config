inputs: final: prev: let
  inherit (final) callPackage;
  inherit (inputs) amded discord mpv-youtube-quality stumpwm unflac;
in {
  amded = callPackage ./amded.nix {src = amded;};
  brave-incognito = callPackage ./brave-incognito.nix {};
  browser = callPackage ./browser.nix {};
  cl-hyperspec = callPackage ./cl-hyperspec.nix {};
  discord = prev.discord.override {src = discord;};
  dmenu = import ./dmenu.nix final prev;
  emacsPackagesFor = import ./epkgs final prev;
  extract_eml = callPackage ./extract_eml.nix {};
  filename_put_duration = callPackage ./filename_put_duration.nix {};
  format_seconds = callPackage ./format_seconds.nix {};
  image-dired-external-viewer = callPackage ./image-dired-external-viewer.nix {};
  image_clipboard = callPackage ./image_clipboard.nix {};
  install_keys = callPackage ./install_keys.nix {};
  make_backup = callPackage ./make_backup.nix {};
  mpv-youtube-quality = callPackage ./mpv-youtube-quality.nix {src = mpv-youtube-quality;};
  mpvi = callPackage ./mpvi.nix {};
  notifiers = callPackage ./notifiers.nix {};
  preparehd = callPackage ./preparehd.nix {};
  qrshow = callPackage ./qrshow.nix {};
  recode_video = callPackage ./recode_video.nix {};
  resize_video = callPackage ./resize_video.nix {};
  select_music_dir = callPackage ./select_music_dir.nix {};
  select_ytdl_fmt = callPackage ./select_ytdl_fmt.nix {};
  sort_videos_by_duration = callPackage ./sort_videos_by_duration.nix {};
  stardicts = callPackage ./stardicts {};
  strip_video = callPackage ./strip_video.nix {};
  stumpwm = callPackage ./stumpwm {src = stumpwm;};
  sudo_askpass = callPackage ./sudo_askpass.nix {};
  unflac = callPackage ./unflac.nix {src = unflac;};
  video_seconds = callPackage ./video_seconds.nix {};
  wallpapers = callPackage ./wallpapers {};
  ytdl = callPackage ./ytdl.nix {};
  ytdla = callPackage ./ytdla.nix {};
  ytdlam = callPackage ./ytdlam.nix {};
  ytdli = callPackage ./ytdli.nix {};
  ytdlp = callPackage ./ytdlp.nix {};
  ytdlpa = callPackage ./ytdlpa.nix {};
  ytdlpam = callPackage ./ytdlpam.nix {};
}
