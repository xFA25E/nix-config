{
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.colorScheme) palette;
  inherit (config.home.sessionVariables) NOTMUCH_CONFIG;
  inherit (lib.strings) escapeShellArg makeBinPath;
  inherit (pkgs) coreutils dbus libnotify notmuch writeShellScript;

  maildir = config.accounts.email.maildirBasePath;
  mailPaths = makeBinPath [libnotify notmuch coreutils dbus];

  mailSync = writeShellScript "mailsync" ''
    export PATH="${mailPaths}''${PATH:+:}$PATH"
    ${config.services.mbsync.package}/bin/mbsync -aV || notify-send "eMail" "Mbsync service failed"
  '';

  mailIndex = writeShellScript "mailindex" ''
    export PATH="${mailPaths}''${PATH:+:}$PATH"
    export NOTMUCH_CONFIG=${escapeShellArg NOTMUCH_CONFIG}
    notmuch new
    count="$(notmuch search tag:unread | wc -l)"
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
          frame_color = "#${palette.base04}";
          frame_width = 3;
          geometry = "0x0-0+0";
          horizontal_padding = 8;
          markup = "full";
          padding = 8;
          separator_color = "frame";
          word_wrap = true;
        };
        urgency_low = {
          background = "#${palette.base00}";
          foreground = "#${palette.base0D}";
        };
        urgency_normal = {
          background = "#${palette.base00}";
          foreground = "#${palette.base04}";
        };
        urgency_critical = {
          background = "#${palette.base00}";
          foreground = "#${palette.base08}";
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
      pinentry.package = pkgs.pinentry;
    };

    grobi = {
      enable = true;
      rules = [
        {
          name = "Veles Dual Monitor";
          outputs_connected = ["eDP-1" "HDMI-2"];
          configure_row = ["HDMI-2" "eDP-1@1368x768"];
          atomic = true;
          primary = "HDMI-2";
          execute_after = ["xrandr --dpi 112"];
        }
        {
          name = "Laptop Single Monitor";
          outputs_connected = ["eDP-1"];
          configure_single = "eDP-1";
          atomic = true;
          primary = "eDP-1";
          execute_after = ["xrandr --dpi 96"];
        }
        {
          name = "Desktop Single Monitor";
          outputs_connected = ["HDMI-0"];
          configure_single = "HDMI-0";
          atomic = true;
          primary = "HDMI-0";
        }
      ];
    };

    mbsync = {
      enable = true;
      postExec = "${mailIndex}";
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

  systemd.user.services.mbsync.Service.ExecStart = lib.mkForce "${mailSync}";
}
