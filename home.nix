{ config, pkgs, ... }:
let colors = import ./colors.nix;
in {
  home.packages = with pkgs; [
    # broken packages
    # qutebrowser libreoffice

    # nixpkgs
    htop checkbashisms dejavu_fonts dmenu fd feh file firefox git hack-font
    iosevka jq ledger leiningen mkpasswd mpc_cli mpd mpop msmtp mtpfs mu p7zip
    pass-otp pinentry pueue pulsemixer pwgen qrencode qtox ripgrep rsync rustup
    sbcl sdcv shellcheck simplescreenrecorder sloccount speedtest-cli stow sxiv
    syncthing tdesktop transmission youtube-dl ungoogled-chromium woof xclip xz
    zip

    # mypkgs
    eldev myProfile myScripts ytdl rimer ungoogledChromiumIncognito sctd
    myStumpwm browser
  ];

  home.extraOutputsToInstall = [ "man" "doc" "info" "devdoc" ];

  programs = {
    emacs = import ./emacs.nix;

    readline = {
      enable = true;
      variables = {
        editing-mode = "emacs";
        blink-matching-paren = true;
      };
    };

    mpv = {
      enable = true;
      config = {
        save-position-on-quit = true;
        watch-later-directory = "~/.cache/mpv/watch_later";
      };
    };

    gpg = {
      enable = true;
      settings = {
        encrypt-to = "Litkovskyy Valeriy <vlr.ltkvsk@protonmail.com>";
      };
    };

    info.enable = true;

    qutebrowser = import ./qutebrowser.nix pkgs colors;
  };

  services = {
    stalonetray = {
      enable = true;
      config = {
        background = colors.base04;
        fuzzy_edges = 3;
        geometry = "1x1+10+742";
        grow_gravity = "SW";
        icon_gravity = "SW";
        icon_size = 16;
        skip_taskbar = true;
        sticky = true;
        transparent = true;
        window_layer = "bottom";
        window_strut = "bottom";
        window_type = "desktop";
      };
    };

    gpg-agent = {
      enable = true;
      defaultCacheTtl = 86400;
      maxCacheTtl = 86400;
    };
  };

  xresources.properties = import ./xresources.nix colors;

  home.file = {
    ".Xresources".onChange = ''
      ${pkgs.xorg.xrdb}/bin/xrdb -load ~/.Xresources
    '';
    ".profile".source = ./profile;
    ".sbclrc".source = ./sbclrc;
    ".shinit".source = ./shinit;
    ".xinitrc".source = ./xinitrc;
  };

  xdg.configFile = {
    "emacs/init.el".source = ./emacs/init.el;
    "emacs/abbrev_defs".source = ./emacs/abbrev_defs;

    "fontconfig/fonts.conf".source = ./fonts.conf;

    "loadkeys/ctrl2caps.map".text = ''
      keymaps 0-2,4-6,8-9,12
      keycode 58 = Control
    '';

    "mpv/scripts/youtube-quality.lua".source = "${pkgs.mpvYoutubeQuality}/youtube-quality.lua";
    "mpv/script-opts/youtube-quality.conf".source = "${pkgs.mpvYoutubeQuality}/youtube-quality.conf";

    "npm/npmrc".text = pkgs.lib.generators.toKeyValue {} {
      prefix = "\${XDG_DATA_HOME}/npm";
      cache = "\${XDG_CACHE_HOME}/npm";
      tmp = "\${XDG_RUNTIME_DIR}/npm";
      init-module = "\${XDG_CONFIG_HOME}/npm/config/npm-init.js";
    };

    "mpop/config".source = ./mpop;
    "msmtp/config".source = ./msmtp;

    "mpd/mpd.conf".text = ''
      music_directory "~/Music"
      db_file         "~/.cache/mpd/database"
      log_file        "~/.cache/mpd/log"
      pid_file        "~/.cache/mpd/pid"
      state_file      "~/.cache/mpd/state"
      sticker_file    "~/.cache/mpd/sticker.sql"
      bind_to_address "localhost"
      port            "6600"
      auto_update     "yes"
      save_absolute_paths_in_playlists "yes"

      audio_output {
        type "pulse"
        name "My Pulse Output"
      }

      volume_normalization "yes"
      filesystem_charset   "UTF-8"
    '';

    "stumpwm".source = ./stumpwm;

    "youtube-dl/config".text = ''
      --add-metadata
      --ignore-errors
      --continue
      --no-playlist
      --embed-subs
      --output "%(uploader)s/%(upload_date)s - %(title)s.%(ext)s"
      --format '(bestvideo+bestaudio/best)[height<=?768][width<=?1366]'
    '';
  } // (if (builtins.pathExists ./emacs/init.elc)
        then { "emacs/init.elc".source = ./emacs/init.elc; }
        else {});

  xdg.dataFile = {
    "stardict/dic".source = "${pkgs.stardictDictionaries}/share/stardict/dic";

    "applications/browser.desktop".text = ''
      [Desktop Entry]
      Categories=Network;WebBrowser;
      Comment=
      Exec=${pkgs.browser}/bin/browser %U
      GenericName=Web Browser
      MimeType=text/html;text/xml;application/xhtml+xml;x-scheme-handler/http;x-scheme-handler/https;x-scheme-handler/ftp
      Name=Browser
      Terminal=false
      Type=Application
    '';
  };

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "val";
  home.homeDirectory = "/home/val";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "21.05";
}
