# Search Engines:
# https://searx.be/?preferences=eJxtVcuO2zoM_ZrrjTFFH4uuvChaXNwBCkzRpN0KtEQrrCXRleRk3K8vnUSOMncWMSKaOjw8fFhDRsuRMHUWA0ZwjYNgZ7DYYXj4sWsca3DroYE5s2Y_OczYWWbrsCEvnmqK_Lx0-zhj4zEf2HTfnnb7JsGACSHqQ_e2yQf02HHSEJuIaXY5KQ4q4Ell6Lt_wSVsDJOSl-yOGDsGOb7haJvzrYeUFyHi2JJmg8fGUILeoVEYLAXJ4N2HDx-flTqSQU7_vP88kh4hJaX8nEiL4ciQlUqsCVzr0RCIcYFgUK5d8xeLJSvAkHJt1Fo_5GOFbik76JWiLIeIxpD4n_VY3yZEMyFGpQZyZ4ujPkJc2hUyUaqxBydM46u3t3A95X7WI-ZrxEsB2snB0no-SgUrZ7949CzBcoSQnBTZ1PES_gng7y1rmVZd2_VxjREWgJt6oG3iuYqSeVw4czrwCOHm57hPGd_EVFAmf_2XMsQ8rR1ThV7gwFwbeMIQceJ0007KTRDW25VIJxrJQIb67oXxxnCIiG3iIZ8gYmsoos4iy2sSruRvOWDkE91pNpjIq6VQOoCUc31cwYo6G70-G7L2dmGI4GFtgqLu75OkVIcoEIVEQdhI5cVzcNLrd8QihZFAV6FfFKYwmEOSXNOh8rzoVRy2gdkENMa2BgcKlInDXdPW6sE0pVdgttQhriWWWT9jzgbDXd7T2HqKkYuWL_jf2JDOf_g-fc-_EMfa0lOw9bnM5jX8u-f_5b8dKdTdg5nZpdcqVBIr0FXwa802kNsolzKWSy_20DqAdMT2fjVYSS6TxzJMVynLssZwvwKNrIj1ZyufbU9Obhav1P0k_-BoRHXgPOKysnySmVOftEYp25enR1nCp0gZ5c1jOFcfVdKRnSu-lxWupBnHbY_3MjRJMpV9njZmJejuvGFWV3355iwqoZN5FMQdukFJII4ezn0mtv_2-2-7isc-gmy_qH58_ypW2XgYG-kTFOi_m0eMQg==&q=%s
# https://duckduckgo.com/?kk=-1&kah=it-it&kl=wt-wt&ks=m&kaj=m&kam=osm&kp=-2&kn=-1&kd=1&kw=s&kak=-1&kax=-1&km=l&q=%s
# https://startpage.com/sp/search?query=hello&prfe=a715a36c09c1472e9d5d804b0ba9312716a96d474575edbfa5e7cb0c646b34216e65fa4ae420b5df58e6c8d3e420eb1771f23caa2663bb5435b01ebb741af66083a80b0bb3682e008b0e7e1126
{
  config,
  pkgs,
  lib,
  ...
}: let
  colors = pkgs.base16Themes.theme "gruvbox-light-medium";
in {
  accounts.email = {
    accounts = {
      "polimi" = {
        address = "valeriy.litkovskyy@mail.polimi.it";
        aliases = ["10622800@polimi.it"];
        imap = {
          host = "outlook.office365.com";
          tls.enable = true;
        };
        mbsync = {
          enable = true;
          groups."polimi" = {
            channels = let
              makeChannel = far: near: {
                extraConfig = {
                  Create = "Slave";
                  Sync = "All";
                  Expunge = "Both";
                  SyncState = "*";
                };
                farPattern = far;
                nearPattern = near;
              };
            in {
              "inbox" = makeChannel "INBOX" "inbox";
              "sent" = makeChannel "Sent Items" "sent";
            };
          };
        };
        msmtp = {
          enable = true;
          extraConfig.logfile = "${config.xdg.cacheHome}/msmtp-polimi.log";
        };
        notmuch.enable = true;
        passwordCommand = let
          pass = "${config.programs.password-store.package}/bin/pass";
          head = "${pkgs.coreutils}/bin/head";
        in "${pass} show mail/polimi | ${head} -n1";
        primary = true;
        realName = "Valeriy Litkovskyy";
        smtp = {
          host = "smtp.office365.com";
          tls = {
            enable = true;
            useStartTls = true;
          };
        };
        userName = "10622800@polimi.it";
      };
    };
    maildirBasePath = "${config.xdg.dataHome}/mail";
  };

  fonts.fontconfig.enable = true;

  gtk = {
    enable = true;
    font = {
      name = "Liberation Sans";
      size = 11;
    };
    gtk2.configLocation = "${config.xdg.cacheHome}/gtk-2.0/gtkrc";
    iconTheme = {
      name = "Papirus-Dark";
      package = pkgs.papirus-icon-theme;
    };
    theme = {
      name = "Gruvbox-Light-Medium";
      package = pkgs.materia-theme.override {
        configBase16 = {
          name = "Gruvbox-Light-Medium";
          kind = "light";
          colors = {
            base00.hex.rgb = colors.base00;
            base01.hex.rgb = colors.base01;
            base02.hex.rgb = colors.base02;
            base03.hex.rgb = colors.base03;
            base04.hex.rgb = colors.base04;
            base05.hex.rgb = colors.base05;
            base06.hex.rgb = colors.base06;
            base07.hex.rgb = colors.base07;
            base08.hex.rgb = colors.base08;
            base09.hex.rgb = colors.base09;
            base0A.hex.rgb = colors.base0A;
            base0B.hex.rgb = colors.base0B;
            base0C.hex.rgb = colors.base0C;
            base0D.hex.rgb = colors.base0D;
            base0E.hex.rgb = colors.base0E;
            base0F.hex.rgb = colors.base0F;
          };
        };
      };
    };
  };

  home = {
    extraOutputsToInstall = ["man" "doc" "info" "devdoc"];

    file = {
      ".stalonetrayrc".text =
        lib.generators.toKeyValue {
          mkKeyValue = lib.generators.mkKeyValueDefault {} " ";
        } {
          background = "\"#000000\"";
          fuzzy_edges = "3";
          geometry = "1x1+10+742";
          grow_gravity = "SW";
          icon_gravity = "SW";
          icon_size = "16";
          skip_taskbar = true;
          sticky = true;
          transparent = true;
          window_layer = "bottom";
          window_strut = "bottom";
          window_type = "desktop";
        };
    };

    keyboard = {
      layout = "dvorak,ru";
      options = ["ctrl:swapcaps" "grp:shifts_toggle"];
      variant = ",ruu";
    };

    language = builtins.listToAttrs (map (n: {
        name = n;
        value = "en_US.UTF-8";
      }) [
        "address"
        "base"
        "collate"
        "ctype"
        "measurement"
        "messages"
        "monetary"
        "name"
        "numeric"
        "paper"
        "telephone"
        "time"
      ]);

    packages = with pkgs; [
      acpi
      alejandra
      alsaUtils
      amded
      ascii
      bind
      binutils
      brave
      brightnessctl
      browser
      calibre
      cloc
      cpulimit
      discord
      djvulibre
      dmenu
      exiftool
      fd
      ffmpeg
      file
      firefox
      ghostscript
      gimp
      go-mtpfs
      hunspell
      hunspellDicts.en_US-large
      hunspellDicts.it_IT
      hunspellDicts.ru_RU
      imagemagick
      iw
      ledger
      leiningen
      libjpeg
      libnotify
      libreoffice
      mediainfo
      mkpasswd
      mpc_cli
      nload
      p7zip
      pandoc
      parted
      pdftk
      perlPackages.JSONPP
      pinentry
      pueue
      pulsemixer
      pwgen
      qrencode
      rar
      ripgrep
      rsync
      scripts
      scrot
      sdcv
      shellcheck
      simplescreenrecorder
      speedtest-cli
      stalonetray
      statix
      stumpwm
      sxiv
      teams
      tor-browser-bundle-bin
      transmission
      unzip
      wget
      woof
      xclip
      xdg-user-dirs
      xkb-switch
      xterm
      xz
      yt-dlp
      zip
      zoom-us
    ];

    sessionVariables = {
      "_JAVA_AWT_WM_NONREPARENTING" = "1";
      ADB_VENDOR_KEY = "${config.xdg.cacheHome}/android";
      ANDROID_SDK_HOME = "${config.xdg.cacheHome}/android";
      BOOT_HOME = "${config.xdg.cacheHome}/boot";
      BROWSER = "browser";
      CARGO_HOME = "${config.xdg.cacheHome}/cargo";
      CUDA_CACHE_PATH = "${config.xdg.cacheHome}/nv";
      EDITOR = "emacs";
      GEM_HOME = "${config.xdg.cacheHome}/gem";
      GEM_SPEC_CACHE = "${config.xdg.cacheHome}/gem";
      GTK_IM_MODULE = "ibus";
      LESSHISFILE = "/dev/null";
      MAILDIR = config.accounts.email.maildirBasePath;
      MPD_HOST = "localhost";
      MPD_PORT = "6600";
      NPM_CONFIG_USERCONFIG = "${config.xdg.configHome}/npm/npmrc";
      QT_IM_MODULE = "ibus";
      RUSTUP_HOME = "${config.xdg.cacheHome}/rustup";
      SSB_HOME = "${config.xdg.cacheHome}/zoom";
      SUDO_ASKPASS = "${pkgs.scripts}/bin/sudo_askpass";
      TERMINAL = "uxterm";
      VISUAL = config.home.sessionVariables.EDITOR;
      WGETRC = "${config.xdg.configHome}/wgetrc";
      XAUTHORITY = "\${XDG_RUNTIME_DIR}/Xauthority";
      XMODIFIERS = "ibus";
      YTDL_DIR = "${config.xdg.userDirs.videos}/youtube";
    };
  };

  programs = {
    bash = {
      enable = true;
      historyControl = ["erasedups" "ignoredups" "ignorespace"];
      historyIgnore = map (cmd: "${cmd}*") [
        "awk"
        "bash"
        "cat"
        "cd"
        "chmod"
        "chown"
        "command"
        "cp"
        "cut"
        "dash"
        "dd"
        "df"
        "dh"
        "du"
        "ebook-convert"
        "echo"
        "emacs"
        "env"
        "exit"
        "export"
        "fd"
        "feh"
        "file"
        "find"
        "gawk"
        "gparted"
        "grep"
        "gzip"
        "hash"
        "host"
        "htop"
        "id"
        "ln"
        "locate"
        "ls"
        "man"
        "mbsync"
        "millisleep"
        "mkdir"
        "mpv"
        "mv"
        "notify-send"
        "ping"
        "pkill"
        "printf"
        "pwd"
        "pwgen"
        "python"
        "quit"
        "read"
        "rg"
        "rm"
        "rmdir"
        "rofi"
        "setsid"
        "sh"
        "sleep"
        "stow"
        "strings"
        "strip"
        "studies_"
        "sxiv"
        "tail"
        "time"
        "timer"
        "top"
        "touch"
        "tr"
        "uname"
        "updatedb"
        "uptime"
        "watch"
        "wc"
        "which"
        "woof"
        "xclip"
        "xz"
        "yay"
        "youtube-dl"
        "yt-dlp"
        "ytdl"
      ];
      profileExtra = ''
        pidof ssh-agent >/dev/null 2>&1 || eval "$(${pkgs.openssh}/bin/ssh-agent)"
      '';
      logoutExtra = ''eval "$(${pkgs.openssh}/bin/ssh-agent -k)"'';
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };

    emacs = {
      enable = true;
      extraPackages = epkgs:
        with epkgs; [
          amded
          async
          avy
          cargo
          consult
          csv-mode
          cyrillic-dvorak-im
          dired-tags
          djvu
          dumb-jump
          ebdb
          edit-indirect
          eglot
          emmet-mode
          envrc
          enwc
          flymake-shellcheck
          flymake-statix
          format-all
          htmlize
          ipretty
          json-navigator
          ledger-mode
          link-hint
          magit
          marginalia
          nix-mode
          nixos-options
          notmuch
          nov
          ob-http
          org
          org-contrib
          org-mime
          org-roam
          pcmpl-args
          pdf-tools
          php-mode
          pueue
          rainbow-mode
          restclient
          reverse-im
          rg
          rust-mode
          rx-widget
          sdcwoc
          skempo
          sly
          sly-asdf
          sly-quicklisp
          sql-indent
          sqlup-mode
          transmission
          tree-sitter
          tree-sitter-langs
          vlf
          web-mode
          wgrep
        ];
      package = pkgs.emacsNativeComp;
    };

    feh.enable = true;

    git = {
      enable = true;
      extraConfig.credential.helper = "cache --timeout=86400";
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
      settings = {
        enable_mouse = 0;
        hide_threads = 1;
        hide_userland_threads = 1;
        highlight_base_name = 1;
        show_cpu_usage = 1;
        show_cpu_frequency = 1;
        show_thread_names = 1;
      };
    };

    info.enable = true;

    jq.enable = true;

    man.generateCaches = true;

    mbsync.enable = true;

    mpv = {
      enable = true;
      bindings = {
        B = "script_message bookmarker-menu";
        b = "script_message bookmarker-quick-save";
        "ctrl+b" = "script_message bookmarker-quick-load";
      };
      config = {
        save-position-on-quit = true;
        watch-later-directory = "${config.xdg.cacheHome}/mpv/watch_later";
        screenshot-directory = "${config.xdg.userDirs.pictures}/mpv";
      };
      profiles = {
        gui = {
          terminal = false;
          force-window = true;
          idle = "once";
        };
      };
    };

    msmtp.enable = true;

    notmuch = {
      enable = true;
      new.tags = ["new"];
      search.excludeTags = ["trash" "spam" "deleted"];
      hooks = {
        preNew = let
          maildir = config.accounts.email.maildirBasePath;
        in ''
          export PATH="${pkgs.findutils}/bin''${PATH:+:}$PATH"

          notmuch search --format=text0 --output=files tag:deleted | xargs -r0 rm -v

          # polimi rules

          mkdir -p '${maildir}'/polimi/{inbox,sent,all}/cur

          ## all tags sent should be in polimi/sent
          notmuch search --output=files --format=text0 tag:polimi AND tag:sent AND NOT 'path:polimi/sent/cur/**' \
            | xargs -r0 -I '{}' mv -v '{}' '${maildir}/polimi/sent/cur'

          ## all tags archive/trash/spam should be in polimi/all
          notmuch search --output=files --format=text0 tag:polimi AND '(tag:archive OR tag:trash OR tag:spam)' AND NOT 'path:polimi/all/cur/**' \
            | xargs -r0 -I '{}' mv -v '{}' '${maildir}/polimi/all/cur'

          ## all tags inbox/flagged should be in polimi/inbox
          notmuch search --output=files --format=text0 tag:polimi AND '(tag:inbox OR tag:flagged)' AND NOT 'path:polimi/inbox/cur/**' \
            | xargs -r0 -I '{}' mv -v '{}' '${maildir}/polimi/inbox/cur'

        '';
        postNew = ''
          # general

          notmuch tag +draft -- 'path:drafts/**'

          # polimi rules

          notmuch tag +polimi -- 'path:polimi/**'
          notmuch tag +inbox -- 'path:polimi/inbox/**'
          notmuch tag +sent  -- 'path:polimi/sent/**'

          ## spam rules

          notmuch tag +spam -inbox -- tag:new AND tag:polimi AND subject:politamtam
          notmuch tag +spam -inbox -- tag:new AND tag:polimi AND subject:"[eventi/events]"
          notmuch tag +spam -inbox -- tag:new AND tag:polimi AND subject:"open day"

          # after processing remove tag new

          notmuch tag -new -- tag:new

        '';
      };
    };

    password-store = {
      enable = true;
      package = pkgs.pass.withExtensions (exts: [exts.pass-otp]);
    };

    readline = {
      enable = true;
      variables = {
        editing-mode = "emacs";
        blink-matching-paren = true;
      };
    };
  };

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

      postExec = let
        mailSync = pkgs.writeShellScript "mailnotify" ''
          export PATH="${lib.strings.makeBinPath (with pkgs; [libnotify notmuch coreutils dbus])}''${PATH:+:}$PATH"
          export NOTMUCH_CONFIG="${config.home.sessionVariables.NOTMUCH_CONFIG}"
          notmuch new
          count="$(notmuch search --output=files tag:unread | wc -l)"
          if test 0 -ne "$count"; then
              notify-send "eMail" "You have $count new emails."
          fi
        '';
      in "${mailSync}";
      preExec = "${pkgs.coreutils}/bin/mkdir -p ${config.accounts.email.maildirBasePath}";
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

  systemd.user.services = {
    pueue = {
      Unit = {
        Description = "Pueue Daemon - CLI process scheduler and manager";
        After = ["graphical-session-pre.target"];
        PartOf = ["graphical-session.target"];
      };
      Service = {
        Restart = "no";
        ExecStart = "${pkgs.pueue}/bin/pueued";
        ExecReload = "${pkgs.pueue}/bin/pueued";
        Environment = "ASYNC_STD_THREAD_COUNT=4";
      };
      Install = {
        WantedBy = ["graphical-session.target"];
      };
    };

    transmission = {
      Unit = {
        Description = "Transmission BitTorrent Daemon";
        After = ["network.target" "graphical-session-pre.target"];
        PartOf = ["graphical-session.target"];
      };
      Service = {
        Type = "notify";
        ExecStart = "${pkgs.transmission}/bin/transmission-daemon -f --log-error";
        ExecReload = "${pkgs.utillinux}/bin/kill -s HUP $MAINPID";
        NoNewPrivileges = true;
      };
      Install = {
        WantedBy = ["graphical-session.target"];
      };
    };

    udiskie.Service.Environment = "PATH=${pkgs.dbus}/bin:${pkgs.systemd}/bin";
  };

  xdg = {
    enable = true;

    configFile = {
      "emacs" = {
        source = builtins.filterSource (p: t: t != "regular" || builtins.match ".*README.org" p == null) ./emacs;
        recursive = true;
      };

      "mpv/scripts/youtube-quality.lua".source = "${pkgs.mpv-youtube-quality}/youtube-quality.lua";
      "mpv/script-opts/youtube-quality.conf".source = "${pkgs.mpv-youtube-quality}/youtube-quality.conf";

      "npm/npmrc".text = lib.generators.toKeyValue {} {
        prefix = "${config.xdg.dataHome}/npm";
        cache = "${config.xdg.cacheHome}/npm";
        init-module = "${config.xdg.configHome}/npm/config/npm-init.js";
        tmp = "\${XDG_RUNTIME_DIR}/npm";
      };

      "stumpwm/config".text = with colors; ''
        (swm-config:init)
        (stumpwm:set-fg-color "${base04}")
        (stumpwm:set-bg-color "${base00}")
        (stumpwm:set-border-color "${base03}")
        (stumpwm:set-focus-color "${base04}")
        (stumpwm:set-unfocus-color "${base00}")
        (setf stumpwm:*colors* '("#${base00}" "#${base08}" "#${base0B}" "#${base0A}" "#${base0D}" "#${base0E}" "#${base0C}" "#${base05}"))
        (mapc #'stumpwm:update-color-map stumpwm:*screen-list*)
      '';

      "wgetrc".text = "hsts-file=${config.xdg.cacheHome}/wget-hsts";

      "yt-dlp/config".text =
        lib.generators.toKeyValue {
          mkKeyValue = lib.generators.mkKeyValueDefault {} " ";
        } {
          "--add-metadata" = "";
          "--compat-options" = "no-live-chat";
          "--continue" = "";
          "--embed-subs" = "";
          "--format" = "'(bestvideo+bestaudio/best)[height<=?768][width<=?1366]/(bestvideo+bestaudio/best)[height<=?1080][width<=?1920]/bestvideo+bestaudio/best'";
          "--ignore-errors" = "";
          "--no-playlist" = "";
          "--output" = "'%(channel,uploader)s - %(upload_date)s - %(title)s.%(ext)s'";
        };
    };

    dataFile = {
      "applications/browser.desktop".text = lib.generators.toINI {} {
        "Desktop Entry" = {
          Categories = "Network;WebBrowser;";
          Comment = "";
          Exec = "${pkgs.browser}/bin/browser %U";
          GenericName = "Web Browser";
          MimeType = "text/html;text/xml;application/xhtml+xml;x-scheme-handler/http;x-scheme-handler/https;x-scheme-handler/ftp";
          Name = "Browser";
          Terminal = false;
          Type = "Application";
        };
      };

      "stardict/dic".source = "${pkgs.stardicts}/share/stardict/dic";
    };

    mimeApps = {
      enable = true;
      associations = {
        added = {
          "application/pdf" = ["emacs.desktop"];
          "application/epub" = ["emacs.desktop"];
        };
        removed = {};
      };
      defaultApplications = {
        "application/pdf" = ["emacs.desktop"];
        "application/epub" = ["emacs.desktop"];
        "text/html" = ["browser.desktop"];
        "x-scheme-handler/https" = ["browser.desktop"];
        "x-scheme-handler/http" = ["browser.desktop"];
      };
    };

    userDirs = let
      home = config.home.homeDirectory;
    in {
      enable = true;
      createDirectories = true;
      desktop = "${home}/Desktop";
      documents = "${home}/Documents";
      download = "${home}/Downloads";
      music = "${home}/Music";
      pictures = "${home}/Pictures";
      videos = "${home}/Videos";
    };
  };

  xresources.properties = import ./xresources.nix colors;

  xsession = {
    enable = true;
    initExtra = ''
      xset +fp ${pkgs.terminus_font}/share/fonts/terminus
    '';
    scriptPath = ".xinitrc";
    windowManager.command = "${pkgs.dbus}/bin/dbus-run-session ${pkgs.stumpwm}/bin/stumpwm";
  };
}
