pkgs: let
  inherit (pkgs.lib.strings) makeBinPath nameFromURL optionalString;

  makeOrangeSound = { url, hash }: pkgs.runCommand (nameFromURL url "." + ".mp3") {
    src = pkgs.fetchzip { inherit url hash; stripRoot = false; };
    nativeBuildInputs = [ pkgs.ffmpeg ];
  } ''
    ffmpeg -i $src/*.mp3 -ss 00:00:00 -to 00:00:01 -c copy $out
  '';

  makeNotificator = name: url: pkgs.writeShellScriptBin name ''
    exec ${pkgs.mpv}/bin/mpv --no-terminal ${makeOrangeSound url}
  '';

  scriptDeps = with pkgs; with pkgs.scripts.scripts; {
    extract_eml = [ mu ];
    filename_put_duration = [ format_seconds video_seconds ];
    format_seconds = [ emacs ];
    image-dired-external-viewer = [ mpv sxiv ];
    image_clipboard = [ xclip ];
    install_keys = [ git gnupg ];
    make_backup = [ rsync ];
    mpvi = [ dmenu mpv youtube-dl ];
    preparehd = [];
    qrshow = [ qrencode sxiv ];
    recode_video_avc = [ ffmpeg ];
    recode_video_hevc = [ ffmpeg ];
    resize_video_144 = [ ffmpeg ];
    resize_video_240 = [ ffmpeg ];
    resize_video_360 = [ ffmpeg ];
    resize_video_480 = [ ffmpeg ];
    sort_videos_by_duration = [ video_seconds ];
    strip_video_mp3 = [ ffmpeg ];
    strip_video_opus = [ ffmpeg ];
    sudo_askpass = [ pass ];
    video_seconds = [ ffmpeg ];
    ytdl = [ filename_put_duration youtube-dl ];
    ytdla = [ ytdl ];
    ytdlam = [ dmenu youtube-dl ];
    ytdli = [ dmenu jq pueue youtube-dl ytdl ytdla ytdlam ];
    ytdlp = [ filename_put_duration youtube-dl ];
    ytdlpa = [ ytdlp ];
  };

  scripts = builtins.mapAttrs (name: deps: pkgs.stdenv.mkDerivation {
    inherit name;
    src = ./. + "/${name}";
    dontUnpack = true;
    dontPatch = true;
    dontConfigure = true;
    dontBuild = true;
    nativeBuildInputs = [ pkgs.makeWrapper ];
    installPhase = ''
      install -DT $src $out/bin/${name}
    '' + optionalString (builtins.length deps != 0) ''
      wrapProgram $out/bin/${name} --prefix PATH : '${makeBinPath deps}'
    '';
  }) scriptDeps // {
    notify_bubble = makeNotificator "notify_bubble" {
      url = "https://www.orangefreesounds.com/wp-content/uploads/2021/03/Bubble-notification-tone.zip";
      hash = "sha256-nw+lVweCm63IGp6AhDJzVQwOxYPRFUz2RoTmvyFXg5g=";
    };
    notify_bruh = makeNotificator "notify_bruh" {
      url = "https://www.orangefreesounds.com/wp-content/uploads/2018/02/Bruh-sound-effect.zip";
      hash = "sha256-CbJ7US6mV88L/+XywCAT1IZ0winIlcFbrRrJiakHHP8=";
    };
  };

in pkgs.symlinkJoin {
  name = "scripts";
  paths = builtins.attrValues scripts;
} // { inherit scripts; }
