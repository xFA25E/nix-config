{ config, pkgs, ... }: let
  variables = import ./variables.nix;
  user = variables.user;
  dir = variables.dir;
  colors = variables.colors;
in {
  home = {
    # enableDebugInfo = true;     # gdb symbols
    extraOutputsToInstall = [ "man" "doc" "info" "devdoc" ];

    file = {
      ".Xresources".onChange = ''
        ${pkgs.xorg.xrdb}/bin/xrdb -load ~/.Xresources || true
      '';
      ".profile".text = ''
        for file in nix.sh hm-session-vars.sh; do
            profile="/home/${user}/.nix-profile/etc/profile.d/''${file}"
            [ -e "''${profile}" ] && . "''${profile}"
        done
      '';
      ".sbclrc".source = ./sbclrc;
      ".shinit".text = ''
        ${pkgs.coreutils}/bin/stty -ixon
        export PS1='$? $USER '
      '';
    };

    homeDirectory = "/home/${user}";

    packages = with pkgs; [
      # nixpkgs
      checkbashisms dejavu_fonts dmenu fd file firefox hack-font iosevka ledger
      leiningen libreoffice mkpasswd mpc_cli mpd mpop mtpfs nix-serve p7zip
      pass-otp pinentry pueue pulsemixer pwgen qrencode qtox ripgrep rsync
      rustup sbcl sdcv shellcheck simplescreenrecorder sloccount speedtest-cli
      sxiv syncthing tdesktop transmission youtube-dl ungoogled-chromium wget
      woof xclip xorg.xbacklight xz zip

      # mypkgs
      browser rimer scripts sctd stumpwm ungoogledChromiumIncognito ytdl
    ];

    sessionPath = [ "${dir.config}/composer/vendor/bin" ];
    sessionVariables = rec {
      # export XDG_DOWNLOAD_DIR="${HOME}/Downloads"
      # export XDG_DOCUMENTS_DIR="${HOME}/Documents"
      # export XDG_MUSIC_DIR="${HOME}/Music"
      # export XDG_PICTURES_DIR="${HOME}/Pictures"
      # export XDG_VIDEOS_DIR="${HOME}/Videos"
      EDITOR = "${pkgs.emacsEditor}/bin/emacseditor";
      VISUAL = EDITOR;
      TERMINAL = "${pkgs.xterm}/bin/uxterm";
      CARGO_HOME = "${dir.cache}/cargo";
      RUSTUP_HOME = "${dir.cache}/rustup";
      LANG = "en_US.UTF-8";
      LESSHISFILE = "/dev/null";
      MU_HOME = "${dir.cache}/mu";
      MAILDIR = "/home/${user}/.mail";
      RIMER_CALLBACK = "${pkgs.scripts}/bin/rimer_callback";
      MPD_HOST = "localhost";
      MPD_PORT = "6600";
      SUDO_ASKPASS = "${pkgs.scripts}/bin/sudo_askpass";
      SSH_ASKPASS = "${pkgs.scripts}/bin/ssh-askpass";
      ENV = "/home/${user}/.shinit";
      "_JAVA_AWT_WM_NONREPARENTING" = "1";
      CUDA_CACHE_PATH = "${dir.cache}/nv";
      NPM_CONFIG_USERCONFIG = "${dir.config}/npm/npmrc";
      SDCV_HISTSIZE = "0";
      HISTFILESIZE = "0";
      HISTSIZE = "0";
      HISTFILE = "";
      SQLITE_HISTORY = "/dev/null";
      MYSQL_HISTFILE = "/dev/null";
      GEM_HOME = "${dir.cache}/gem";
      GEM_SPEC_CACHE = "${dir.cache}/gem";
      ANDROID_SDK_HOME = "${dir.cache}/android";
      ADB_VENDOR_KEY = "${dir.cache}/android";
      BOOT_HOME = "${dir.cache}/boot";
      YTDL_DIR="${dir.videos}/youtube";
      BROWSER = "${pkgs.browser}/bin/browser";
      GTK2_RC_FILES = "${dir.cache}/gtk-2.0/gtkrc";
      GTK_IM_MODULE = "ibus";
      QT_IM_MODULE = "ibus";
      XMODIFIERS = "ibus";
      XAUTHORITY = "\${XDG_RUNTIME_DIR}/Xauthority";
      SSB_HOME = "${dir.cache}/zoom";
    };

    stateVersion = "21.03";
    username = user;
  };

  programs = {
    emacs = {
      enable = true;
      package = pkgs.myEmacs;
    };

    feh.enable = true;

    git = {
      enable = true;
      ignores = [ "*.elc" ];
      # signing = {
      #   key = "vlr.ltkvsk@protonmail.com";
      #   signByDefault = true;
      # };
      userEmail = "vlr.ltkvsk@protonmail.com";
      userName = "Valeriy Litkovskyy";
    };

    gpg = {
      enable = true;
      settings = {
        encrypt-to = "Litkovskyy Valeriy <vlr.ltkvsk@protonmail.com>";
      };
    };

    home-manager.enable = true;

    htop = {
      enable = true;
      enableMouse = false;
      hideThreads = true;
      hideUserlandThreads = true;
      highlightBaseName = true;
      meters.right = [ "Tasks" "LoadAverage" "Uptime" "Battery" ];
      showCpuUsage = true;
      showCpuFrequency = true;
      showThreadNames = true;
    };

    info.enable = true;

    jq.enable = true;

    man.generateCaches = true;

    mpv = {
      enable = true;
      config = {
        save-position-on-quit = true;
        watch-later-directory = "${dir.cache}/mpv/watch_later";
      };
    };

    msmtp = {
      enable = true;
      extraConfig = import ./msmtp.nix pkgs dir;
    };

    mu.enable = true;

    qutebrowser = import ./qutebrowser.nix pkgs colors;

    readline = {
      enable = true;
      variables = {
        editing-mode = "emacs";
        blink-matching-paren = true;
      };
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      defaultCacheTtl = 86400;
      maxCacheTtl = 86400;
      verbose = true;
    };
  };

  xdg = {
    enable = true;

    cacheHome = dir.cache;

    configFile = {
      "emacs" = {
        source = ./emacs;
        recursive = true;
      };

      "fontconfig/fonts.conf".source = ./fonts.conf;

      "loadkeys/ctrl2caps.map".text = ''
        keymaps 0-2,4-6,8-9,12
        keycode 58 = Control
      '';

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

      "mpop/config".text = import ./mpop.nix pkgs dir;

      "mpv/scripts/youtube-quality.lua".source = "${pkgs.mpvYoutubeQuality}/youtube-quality.lua";
      "mpv/script-opts/youtube-quality.conf".source = "${pkgs.mpvYoutubeQuality}/youtube-quality.conf";

      "npm/npmrc".text = pkgs.lib.generators.toKeyValue {} {
        prefix = "${dir.data}/npm";
        cache = "${dir.cache}/npm";
        init-module = "${dir.config}/npm/config/npm-init.js";
        tmp = "\${XDG_RUNTIME_DIR}/npm";
      };

      "stumpwm" = {
        source = ./stumpwm;
        recursive = true;
      };

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

    configHome = dir.config;

    dataFile = {
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

      "applications/emacseditor.desktop".text = ''
        [Desktop Entry]
        Name=Emacs
        GenericName=Text Editor
        Comment=Edit text
        MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
        Exec=${pkgs.emacsEditor}/bin/emacseditor %F
        Icon=emacs
        Type=Application
        Terminal=false
        Categories=Development;TextEditor;
        StartupWMClass=Emacs
        Keywords=Text;Editor;
      '';

      "stardict/dic".source = "${pkgs.stardictDictionaries}/share/stardict/dic";
    };

    dataHome = dir.data;

    mimeApps = {
      enable = true;
      associations = {
        added = {
          "application/pdf" = [ "emacseditor.desktop" ];
          "application/epub" = [ "emacseditor.desktop" ];
        };
        removed = {};
      };
      defaultApplications = {
        "application/pdf" = [ "emacseditor.desktop" ];
        "application/epub" = [ "emacseditor.desktop" ];
      };
    };

    userDirs = {
      enable = true;
      music = dir.music;
      videos = dir.videos;
    };
  };

  xresources.properties = import ./xresources.nix colors;

  xsession = {
    enable = true;
    scriptPath = ".xinitrc";
    initExtra = ''
      ${pkgs.xorg.xrdb}/bin/xrdb -load ~/.Xresources
      pidof rimer >/dev/null || setsid -f ${pkgs.rimer}/bin/rimer start "${pkgs.scripts}/bin/rimer_callback"
      pidof pueued >/dev/null || setsid -f ${pkgs.pueue}/bin/pueued
      pkill sctd; setsid -f ${pkgs.sctd}/bin/sctd --longitude 47.339 --latitude 8.877
    '';
    windowManager.command = "${pkgs.stumpwm}/bin/stumpwm";
  };
}
