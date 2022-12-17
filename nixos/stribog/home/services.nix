{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.colorScheme) colors;
  inherit (config.home.sessionVariables) NOTMUCH_CONFIG;
  inherit (lib.strings) escapeShellArg makeBinPath;
  inherit (pkgs) coreutils dbus libnotify notmuch writeShellScript;

  maildir = config.accounts.email.maildirBasePath;
  mailSync = writeShellScript "mailnotify" ''
    export PATH="${makeBinPath [libnotify notmuch coreutils dbus]}''${PATH:+:}$PATH"
    export NOTMUCH_CONFIG=${escapeShellArg NOTMUCH_CONFIG}
    notmuch new
    count="$(notmuch search --output=files tag:unread | wc -l)"
    if test 0 -ne "$count"; then
      notify-send "eMail" "You have $count new emails."
    fi
  '';
in {
  services = {
    dunst = {
      enable = true;
      settings = {
        global = {
          browser = "${pkgs.browser}/bin/browser";
          dmenu = "${pkgs.dmenu}/bin/dmenu";
          font = "monospace 15";
          frame_color = "#${colors.base04}";
          frame_width = 3;
          geometry = "0x0-0+0";
          horizontal_padding = 8;
          markup = "full";
          padding = 8;
          separator_color = "frame";
          word_wrap = true;
        };
        urgency_low = {
          background = "#${colors.base00}";
          foreground = "#${colors.base0D}";
        };
        urgency_normal = {
          background = "#${colors.base00}";
          foreground = "#${colors.base04}";
        };
        urgency_critical = {
          background = "#${colors.base00}";
          foreground = "#${colors.base08}";
        };
      };
    };

    gammastep = {
      enable = true;
      latitude = "8.877";
      longitude = "47.339";
    };

    gpg-agent = {
      enable = true;
      defaultCacheTtl = 86400;
      maxCacheTtl = 86400;
      verbose = true;
    };

    mbsync = {
      enable = true;
      postExec = "${mailSync}";
      preExec = "${coreutils}/bin/mkdir -p ${maildir}";
      verbose = true;
    };

    mpd = {
      enable = true;
      dataDir = "${config.xdg.cacheHome}/mpd";
      musicDirectory = config.xdg.userDirs.music;
      extraConfig = ''
        audio_output {
          type "pulse"
          name "My Pulse Output"
          mixer_type "software"
        }

        filesystem_charset   "UTF-8"
        volume_normalization "yes"
      '';
      network.startWhenNeeded = true;
    };

    random-background = {
      enable = true;
      imageDirectory = "${pkgs.wallpapers}";
      interval = "5min";
    };

    syncthing = {
      enable = true;
      tray.enable = false;
    };

    udiskie.enable = true;
    unclutter.enable = true;
  };
}
