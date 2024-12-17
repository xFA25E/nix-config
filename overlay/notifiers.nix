{
  fetchzip,
  ffmpeg,
  mpg123,
  runCommand,
  symlinkJoin,
  writeShellScriptBin,
}: let
  makeOrangeSound = {
    name,
    url,
    hash,
  }:
    runCommand "${name}.mp3" {
      src = fetchzip {
        inherit url hash;
        stripRoot = false;
      };
      nativeBuildInputs = [ffmpeg];
    } ''
      ffmpeg -i $src/*.mp3 -ss 00:00:00 -to 00:00:01 -c copy $out
    '';

  makeNotifier = {name, ...} @ src:
    writeShellScriptBin "notify_${name}" ''
      exec ${mpg123}/bin/mpg123 -q ${makeOrangeSound src} >/dev/null 2>/dev/null
    '';
in
  symlinkJoin {
    name = "notifiers";
    paths = map makeNotifier [
      {
        name = "bubble";
        url = "https://www.orangefreesounds.com/wp-content/uploads/2021/03/Bubble-notification-tone.zip";
        hash = "sha256-nw+lVweCm63IGp6AhDJzVQwOxYPRFUz2RoTmvyFXg5g=";
      }
      {
        name = "bruh";
        url = "https://www.orangefreesounds.com/wp-content/uploads/2018/02/Bruh-sound-effect.zip";
        hash = "sha256-CbJ7US6mV88L/+XywCAT1IZ0winIlcFbrRrJiakHHP8=";
      }
      {
        name = "ding";
        url = "https://www.orangefreesounds.com/wp-content/uploads/Zip/Ding-sound.zip";
        hash = "sha256-pi1Txpq/8R+/piDwK1Ky+SVAosI3uGe/8R5Sp2CFnug=";
      }
      {
        name = "church_bell";
        url = "https://orangefreesounds.com/wp-content/uploads/2023/06/Sound-effect-of-a-church-bell-ringing-in-a-mediterranean-village.zip";
        hash = "sha256-2622yU+UIhjsQPEP2bWse+ydCcTmvG0rf0AQRyE+J7I=";
      }
    ];
  }
