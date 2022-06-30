pkgs: let
  inherit (pkgs.lib.strings) makeBinPath optionalString;

  bruh_sound_effect_2 = pkgs.runCommand "bruh_sound_effect_2.mp3" {
    src = pkgs.fetchzip {
      stripRoot = false;
      url = "https://www.orangefreesounds.com/wp-content/uploads/2018/02/Bruh-sound-effect.zip";
      sha256 = "sha256-CbJ7US6mV88L/+XywCAT1IZ0winIlcFbrRrJiakHHP8=";
    };
    nativeBuildInputs = [ pkgs.ffmpeg ];
  } ''
    ffmpeg -i $src/Bruh-sound-effect.mp3 -ss 00:00:00 -to 00:00:01 -c copy $out
  '';

  bubble_notification_tone = pkgs.runCommand "bubble_notification_tone.mp3" {
    src = pkgs.fetchzip {
      stripRoot = false;
      url = "https://www.orangefreesounds.com/wp-content/uploads/2021/03/Bubble-notification-tone.zip";
      sha256 = "sha256-nw+lVweCm63IGp6AhDJzVQwOxYPRFUz2RoTmvyFXg5g=";
    };
    nativeBuildInputs = [ pkgs.ffmpeg ];
  } ''
    ffmpeg -i $src/Bubble-notification-tone.mp3 -ss 00:00:00 -to 00:00:01 -c copy $out
  '';


  scriptDeps = with pkgs; with pkgs.scripts.scripts; {
    extract_eml = [ mu ];
    filename_put_duration = [ format_duration video_duration ];
    format_duration = [];
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
    sort_videos_by_duration = [ video_duration ];
    strip_video_mp3 = [ ffmpeg ];
    strip_video_opus = [ ffmpeg ];
    sudo_askpass = [ pass ];
    video_duration = [ ffmpeg ];
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
    notify_bruh = pkgs.writeShellScriptBin "notify_bruh" ''
      exec ${pkgs.mpv}/bin/mpv --no-terminal ${bruh_sound_effect_2}
    '';
    notify_bubble = pkgs.writeShellScriptBin "notify_bubble" ''
      exec ${pkgs.mpv}/bin/mpv --no-terminal ${bubble_notification_tone}
    '';
  };

in pkgs.symlinkJoin {
  name = "scripts";
  paths = builtins.attrValues scripts;
} // { inherit scripts; }
