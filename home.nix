{ config, pkgs, ... }: let
  variables = import ./variables.nix;
  user = variables.user;
  dir = variables.dir;
  colors = variables.colors;
in {
  home = {
    username = user;
    homeDirectory = "/home/${user}";
    stateVersion = "21.03";

    packages = with pkgs; [
      # broken packages
      # qutebrowser

      # nixpkgs
      htop checkbashisms dejavu_fonts dmenu fd feh file firefox git hack-font
      iosevka jq ledger leiningen libreoffice mkpasswd mpc_cli mpd mpop msmtp
      mtpfs mu p7zip pass-otp pinentry pueue pulsemixer pwgen qrencode qtox
      ripgrep rsync rustup sbcl sdcv shellcheck simplescreenrecorder sloccount
      speedtest-cli stow sxiv syncthing tdesktop transmission youtube-dl
      ungoogled-chromium woof xclip xz zip

      # mypkgs
      browser myEmacs rimer scripts sctd stumpwm ungoogledChromiumIncognito
      userProfile ytdl
    ];

    extraOutputsToInstall = [ "man" "doc" "info" "devdoc" ];

    file = {
      ".Xresources".onChange = ''
        ${pkgs.xorg.xrdb}/bin/xrdb -load ~/.Xresources || true
      '';
      ".profile".source = ./profile;
      ".sbclrc".source = ./sbclrc;
      ".shinit".text = ''
        ${pkgs.coreutils}/bin/stty -ixon
        PS1='$? $(${pkgs.coreutils}/bin/whoami) '
      '';
      ".xinitrc".source = ./xinitrc;
    };
  };

  xdg = {
    configFile = {
      "fontconfig/fonts.conf".source = ./fonts.conf;

      "loadkeys/ctrl2caps.map".text = ''
        keymaps 0-2,4-6,8-9,12
        keycode 58 = Control
      '';

      "mpv/scripts/youtube-quality.lua".source = "${pkgs.mpvYoutubeQuality}/youtube-quality.lua";
      "mpv/script-opts/youtube-quality.conf".source = "${pkgs.mpvYoutubeQuality}/youtube-quality.conf";

      "npm/npmrc".text = pkgs.lib.generators.toKeyValue {} {
        prefix = "${dir.data}/npm";
        cache = "${dir.cache}/npm";
        init-module = "${dir.config}/npm/config/npm-init.js";
        tmp = "\${XDG_RUNTIME_DIR}/npm";
      };

      "mpop/config".text = import ./mpop.nix pkgs dir;
      "msmtp/config".text = import ./msmtp.nix pkgs dir;

      "mpd/mpd.conf".text = ''
        music_directory "${dir.music}"
        db_file         "${dir.cache}/mpd/database"
        log_file        "${dir.cache}/mpd/log"
        pid_file        "${dir.cache}/mpd/pid"
        state_file      "${dir.cache}/mpd/state"
        sticker_file    "${dir.cache}/mpd/sticker.sql"
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
    };

    dataFile = {
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
  };

  programs = {
    home-manager.enable = true;

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
        watch-later-directory = "${dir.cache}/mpv/watch_later";
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
}
