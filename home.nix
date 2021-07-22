# Search Engines:
# https://searx.be/?preferences=eJxtVcuO2zoM_ZrrjTFFH4uuvChaXNwBCkzRpN0KtEQrrCXRleRk3K8vnUSOMncWMSKaOjw8fFhDRsuRMHUWA0ZwjYNgZ7DYYXj4sWsca3DroYE5s2Y_OczYWWbrsCEvnmqK_Lx0-zhj4zEf2HTfnnb7JsGACSHqQ_e2yQf02HHSEJuIaXY5KQ4q4Ell6Lt_wSVsDJOSl-yOGDsGOb7haJvzrYeUFyHi2JJmg8fGUILeoVEYLAXJ4N2HDx-flTqSQU7_vP88kh4hJaX8nEiL4ciQlUqsCVzr0RCIcYFgUK5d8xeLJSvAkHJt1Fo_5GOFbik76JWiLIeIxpD4n_VY3yZEMyFGpQZyZ4ujPkJc2hUyUaqxBydM46u3t3A95X7WI-ZrxEsB2snB0no-SgUrZ7949CzBcoSQnBTZ1PES_gng7y1rmVZd2_VxjREWgJt6oG3iuYqSeVw4czrwCOHm57hPGd_EVFAmf_2XMsQ8rR1ThV7gwFwbeMIQceJ0007KTRDW25VIJxrJQIb67oXxxnCIiG3iIZ8gYmsoos4iy2sSruRvOWDkE91pNpjIq6VQOoCUc31cwYo6G70-G7L2dmGI4GFtgqLu75OkVIcoEIVEQdhI5cVzcNLrd8QihZFAV6FfFKYwmEOSXNOh8rzoVRy2gdkENMa2BgcKlInDXdPW6sE0pVdgttQhriWWWT9jzgbDXd7T2HqKkYuWL_jf2JDOf_g-fc-_EMfa0lOw9bnM5jX8u-f_5b8dKdTdg5nZpdcqVBIr0FXwa802kNsolzKWSy_20DqAdMT2fjVYSS6TxzJMVynLssZwvwKNrIj1ZyufbU9Obhav1P0k_-BoRHXgPOKysnySmVOftEYp25enR1nCp0gZ5c1jOFcfVdKRnSu-lxWupBnHbY_3MjRJMpV9njZmJejuvGFWV3355iwqoZN5FMQdukFJII4ezn0mtv_2-2-7isc-gmy_qH58_ypW2XgYG-kTFOi_m0eMQg==&q=%s
# https://duckduckgo.com/?kk=-1&kah=it-it&kl=wt-wt&ks=m&kaj=m&kam=osm&kp=-2&kn=-1&kd=1&kw=s&kak=-1&kax=-1&km=l&q=%s

{ config, pkgs, ... }: let
  variables = import ./variables.nix;
  user = variables.user;
  dir = variables.dir;
  colors = variables.colors;
in {
  accounts.email = {
    accounts = {
      "polimi" = {
        address = "valeriy.litkovskyy@mail.polimi.it";
        aliases = [ "10622800@polimi.it" ];
        imap = {
          host = "outlook.office365.com";
          tls.enable = true;
        };
        mbsync = {
          enable = true;
          groups."polimi" = {
            channels = let makeChannel = master: slave: {
              extraConfig = {
                Create = "Slave";
                Sync = "All";
                Expunge = "Both";
                SyncState = "*";
              };
              masterPattern = master;
              slavePattern = slave;
            }; in {
              "inbox" = makeChannel "INBOX" "inbox";
              "sent" = makeChannel "Sent Items" "sent";
            };
          };
        };
        msmtp = {
          enable = true;
          extraConfig.logfile = "${dir.cache}/msmtp-polimi.log";
        };
        notmuch.enable = true;
        passwordCommand = "${pkgs.pass}/bin/pass show mail/polimi | ${pkgs.coreutils}/bin/head -n1";
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
    maildirBasePath = dir.mail;
  };

  fonts.fontconfig.enable = true;
  home = {
    # enableDebugInfo = true;     # gdb symbols
    extraOutputsToInstall = [ "man" "doc" "info" "devdoc" ];

    file = {
      ".abclrc".source = ./common-lisp.lisp;
      ".ccl-init.lisp".source = ./common-lisp.lisp;
      ".clisprc.lisp".source = ./common-lisp.lisp;
      ".eclrc".source = ./common-lisp.lisp;
      ".Xresources".onChange = ''
        ${pkgs.xorg.xrdb}/bin/xrdb -load ~/.Xresources || true
      '';
      ".profile".text = ''
        test -r "/home/${user}/.nix-profile/etc/profile.d/nix.sh" && . "/home/${user}/.nix-profile/etc/profile.d/nix.sh"
        test -r "/home/${user}/.nix-profile/etc/profile.d/hm-session-vars.sh" && . "/home/${user}/.nix-profile/etc/profile.d/hm-session-vars.sh"

        test -r "/home/${user}/.guix-profile/etc/profile" && . "/home/${user}/.guix-profile/etc/profile"
        test -r "${dir.config}/guix/current/etc/profile" && . "${dir.config}/guix/current/etc/profile"

        eval `${pkgs.openssh}/bin/ssh-agent`
      '';
      ".sbclrc".source = ./common-lisp.lisp;
      ".shinit".text = ''
        ${pkgs.coreutils}/bin/stty -ixon
        PS1='[$USER $?] $(test $UID -eq 0 && echo "#" || echo "$") '
      '';
      ".stalonetrayrc".text = pkgs.lib.generators.toKeyValue {
        mkKeyValue = pkgs.lib.generators.mkKeyValueDefault {} " ";
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

    homeDirectory = "/home/${user}";

    keyboard = {
      layout = "dvorak,ru";
      options = [ "ctrl:swapcaps" "grp:shifts_toggle" ];
      variant = ",ruu";
    };

    language = {
      address = "en_US.UTF-8";
      base = "en_US.UTF-8";
      collate = "en_US.UTF-8";
      ctype = "en_US.UTF-8";
      measurement = "en_US.UTF-8";
      messages = "en_US.UTF-8";
      monetary = "en_US.UTF-8";
      name = "en_US.UTF-8";
      numeric = "en_US.UTF-8";
      paper = "en_US.UTF-8";
      telephone = "en_US.UTF-8";
      time = "en_US.UTF-8";
    };

    packages = with pkgs; [

      acpi alsaUtils ascii bind brightnessctl brave braveIncognito browser
      calibre cpulimit dmenu fd firefox ffmpeg file gimp hunspell
      hunspellDicts.en_US-large hunspellDicts.it_IT hunspellDicts.ru_RU
      imagemagick iw ledger leiningen libnotify libreoffice mediainfo mkpasswd
      mpc_cli myEmacs nload p7zip pass-otp pdftk perlPackages.JSONPP pinentry
      pueue pulsemixer pwgen qrencode ripgrep rsync scripts scrot sdcv
      simplescreenrecorder sloccount speedtest-cli stalonetray stumpwm sxiv
      syncthing tdesktop transmission unzip wget woof xclip xdg-user-dirs xterm
      xz youtube-dl ytdl zip zoom-us

      go-mtpfs parted exiftool libjpeg shellcheck
      discord
      # torbrowser

    ];

    sessionPath = [ "${dir.config}/composer/vendor/bin" "${dir.data}/npm/bin" "/usr/local/bin" ];
    sessionVariables = {
      EDITOR = "emacs";
      VISUAL = "emacs";
      TERMINAL = "uxterm";
      LESSHISFILE = "/dev/null";
      MAILDIR = "${dir.mail}";
      MPD_HOST = "localhost";
      MPD_PORT = "6600";
      SUDO_ASKPASS = "${pkgs.scripts}/bin/sudo_askpass";
      ENV = "/home/${user}/.shinit";
      "_JAVA_AWT_WM_NONREPARENTING" = "1";
      CUDA_CACHE_PATH = "${dir.cache}/nv";
      NPM_CONFIG_USERCONFIG = "${dir.config}/npm/npmrc";
      GEM_HOME = "${dir.cache}/gem";
      GEM_SPEC_CACHE = "${dir.cache}/gem";
      ANDROID_SDK_HOME = "${dir.cache}/android";
      ADB_VENDOR_KEY = "${dir.cache}/android";
      BOOT_HOME = "${dir.cache}/boot";
      YTDL_DIR="${dir.videos}/youtube";
      BROWSER = "browser";
      GTK2_RC_FILES = "${dir.cache}/gtk-2.0/gtkrc";
      GTK_IM_MODULE = "ibus";
      QT_IM_MODULE = "ibus";
      XMODIFIERS = "ibus";
      XAUTHORITY = "\${XDG_RUNTIME_DIR}/Xauthority";
      SSB_HOME = "${dir.cache}/zoom";
      LOCATE_PATH = "${dir.cache}/locatedb";
      RUSTUP_HOME = "${dir.cache}/rustup";
      CARGO_HOME = "${dir.cache}/cargo";
      QUICKLISP = "${dir.cache}/quicklisp";
      ELDEV_DIR = "${dir.cache}/eldev";
      INFOPATH = "$INFOPATH\${INFOPATH:+:}/usr/local/share/info";
      WGETRC = "${dir.config}/wgetrc";
    };

    stateVersion = "21.03";
    username = user;
  };

  programs = {
    bash = {
      enable = true;
      historyControl = [ "erasedups" "ignoredups" "ignorespace" ];
      historyIgnore = map (cmd: "${cmd}*") [

        "awk" "bash" "cat" "cd" "chmod" "chown" "command" "cp" "cut" "dash" "dd"
        "df" "dh" "du" "ebook-convert" "echo" "emacs" "env" "exit" "export" "fd"
        "feh" "file" "find" "gawk" "gparted" "grep" "gzip" "hash" "host" "htop"
        "id" "ln" "locate" "ls" "man" "mbsync" "millisleep" "mkdir" "mpv" "mv"
        "notify-send" "ping" "pkill" "printf" "pwd" "pwgen" "python" "quit"
        "read" "rg" "rm" "rmdir" "rofi" "setsid" "sh" "sleep" "stow" "strings"
        "strip" "studies_" "sxiv" "tail" "time" "timer" "top" "touch" "tr"
        "uname" "uptime" "watch" "wc" "which" "woof" "xclip" "xz" "yay"
        "youtube-dl" "ytdl"

      ];
      initExtra = ''
        [ -n "$ENV" ] && . "$ENV"

        for f in ${dir.config}/guix/current/etc/bash_completion.d/* ; do
            test -r "$f" && . "$f"
        done
      '';
    };

    direnv = {
      config = {
        Whitelist = {
          prefix = [ "/home/${user}/Documents/projects/" ];
        };
      };
      enable = true;
      enableBashIntegration = true;
      enableNixDirenvIntegration = true;
    };

    feh.enable = true;

    git = {
      enable = true;
      extraConfig.credential.helper = "cache --timeout=86400";
      ignores = [ "*.elc" ".direnv" ".eldev" ];
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

    mbsync.enable = true;

    mpv = {
      enable = true;
      config = {
        save-position-on-quit = true;
        watch-later-directory = "${dir.cache}/mpv/watch_later";
        screenshot-directory = "${dir.pictures}/mpv";
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
      new.tags = [ "new" ];
      search.excludeTags = [ "trash" "spam" "deleted" ];
      hooks = {
        preNew = ''
          export PATH="${pkgs.findutils}/bin''${PATH:+:}$PATH"

          notmuch search --format=text0 --output=files tag:deleted | xargs -r0 rm -v

          # polimi rules

          mkdir -p '${dir.mail}'/polimi/{inbox,sent,all}/cur

          ## all tags inbox/todo/flagged should be in polimi/inbox
          notmuch search --output=files --format=text0 tag:polimi AND '(tag:inbox OR tag:todo OR tag:flagged)' AND NOT 'path:polimi/inbox/cur/**' \
            | xargs -r0 -I '{}' mv -v '{}' '${dir.mail}/polimi/inbox/cur'

          ## all tags sent should be in polimi/sent
          notmuch search --output=files --format=text0 tag:polimi AND tag:sent AND NOT 'path:polimi/sent/cur/**' \
            | xargs -r0 -I '{}' mv -v '{}' '${dir.mail}/polimi/sent/cur'

          ## all tags archive/trash/spam should be in polimi/all
          notmuch search --output=files --format=text0 tag:polimi AND '(tag:archive OR tag:trash OR tag:spam)' AND NOT 'path:polimi/all/cur/**' \
            | xargs -r0 -I '{}' mv -v '{}' '${dir.mail}/polimi/all/cur'

        '';
        postNew = ''
          # general

          notmuch tag +draft -- 'path:drafts/**'

          # polimi rules

          notmuch tag +polimi -- 'path:polimi/**'
          notmuch tag +inbox -- 'path:polimi/inbox/**'
          notmuch tag +sent  -- 'path:polimi/sent/**'
          notmuch tag +todo -inbox -sent -- tag:inbox and tag:sent

          ## spam rules

          notmuch tag +spam -inbox -- tag:new AND tag:polimi AND subject:politamtam
          notmuch tag +spam -inbox -- tag:new AND tag:polimi AND subject:"[eventi/events]"
          notmuch tag +spam -inbox -- tag:new AND tag:polimi AND subject:"open day"

          # after processing remove tag new

          notmuch tag -new -- tag:new

        '';
      };
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
          geometry = "0x0-0+0";
          dmenu = "${pkgs.dmenu}/bin/dmenu";
          browser = "${pkgs.browser}/bin/browser";
          padding = 8;
          horizontal_padding = 8;
          frame_width = 3;
          frame_color = colors.base04;
          separator_color = "frame";
          font = "monospace 15";
          markup = "full";
          word_wrap = true;
        };
        urgency_low = {
          background = colors.base00;
          foreground = colors.base0D;
        };
        urgency_normal = {
          background = colors.base00;
          foreground = colors.base04;
        };
        urgency_critical = {
          background = colors.base00;
          foreground = colors.base08;
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

    # grobi.enable = true; # does not work on greypc. autorandr?

    mbsync = {
      enable = true;

      postExec = let
        mailSync = pkgs.writeShellScript "mailnotify" ''
          export PATH="${with pkgs; lib.strings.makeBinPath [libnotify notmuch coreutils dbus]}''${PATH:+:}$PATH"
          export NOTMUCH_CONFIG="${config.home.sessionVariables.NOTMUCH_CONFIG}"
          notmuch new
          count="$(notmuch search --output=files tag:unread | wc -l)"
          if test 0 -ne "$count"; then
              notify-send "eMail" "You have $count new emails."
          fi
        '';
      in "${mailSync}";
      preExec = "${pkgs.coreutils}/bin/mkdir -p ${dir.mail}";
      verbose = true;
    };

    mpd = {
      enable = true;
      dataDir = "${dir.cache}/mpd";
      musicDirectory = dir.music;
      extraConfig = ''
        audio_output {
          type "pulse"
          name "My Pulse Output"
          mixer_type "software"
        }

        volume_normalization "yes"
        filesystem_charset   "UTF-8"
      '';
      network.startWhenNeeded = true;
    };

    random-background = {
      enable = true;
      imageDirectory = "${pkgs.wallpapers}";
      interval = "5min";
    };

    udiskie.enable = true;
    unclutter.enable = true;
  };

  systemd.user = {
    timers = {
      updatedb = {
        Unit = {
          Description = "Updabedb timer";
        };
        Timer = {
          Unit = "updatedb.service";
          OnCalendar = "13:00";
        };
        Install = {
          WantedBy = [ "timers.target" ];
        };
      };
    };

    services = {
      updatedb = {
        Unit = {
          Description = "Updatedb - filesystem database";
        };
        Service = {
          Environment = [ "PATH=${pkgs.gnused}/bin:${pkgs.coreutils}/bin" ];
          ExecStart = "${pkgs.findutils}/bin/updatedb --output=${dir.cache}/locatedb";
          IOSchedulingClass = "idle";
          Type = "oneshot";
        };
      };

      pueue = {
        Unit = {
          Description = "Pueue Daemon - CLI process scheduler and manager";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          Restart = "no";
          ExecStart = "${pkgs.pueue}/bin/pueued";
          ExecReload = "${pkgs.pueue}/bin/pueued";
          Environment = "ASYNC_STD_THREAD_COUNT=4";
        };
        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };

      transmission = {
        Unit = {
          Description = "Transmission BitTorrent Daemon";
          After = [ "network.target" "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          Type = "notify";
          ExecStart = "${pkgs.transmission}/bin/transmission-daemon -f --log-error";
          ExecReload = "${pkgs.utillinux}/bin/kill -s HUP $MAINPID";
          NoNewPrivileges = true;
        };
        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };

      xrdb = {
        Unit = {
          Description = "Load Xresources with xrdb";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          ExecStart = "${pkgs.xorg.xrdb}/bin/xrdb -load %h/.Xresources";
          IOSchedulingClass = "idle";
          Type = "oneshot";
        };
        Install = {
          WantedBy = [ "graphical-session.target" ];
        };
      };
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

      "wgetrc".text = "hsts-file=${dir.cache}/wget-hsts";

      "youtube-dl/config".text = pkgs.lib.generators.toKeyValue {
        mkKeyValue = pkgs.lib.generators.mkKeyValueDefault {} " ";
      } {
        "--add-metadata" = "";
        "--ignore-errors" = "";
        "--continue" = "";
        "--no-playlist" = "";
        "--embed-subs" = "";
        "--output" = "'%(uploader)s - %(upload_date)s - %(title)s.%(ext)s'";
        "--format" = "'(bestvideo+bestaudio/best)[height<=?768][width<=?1366]'";
      };
    };

    configHome = dir.config;

    dataFile = {
      "applications/browser.desktop".text = pkgs.lib.generators.toINI {} {
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

      "stardict/dic".source = "${pkgs.stardictDictionaries}/share/stardict/dic";
    };

    dataHome = dir.data;

    mimeApps = {
      enable = true;
      associations = {
        added = {
          "application/pdf" = [ "emacs.desktop" ];
          "application/epub" = [ "emacs.desktop" ];
          "x-scheme-handler/tg" = [ "userapp-Telegram Desktop-1GLA50.desktop" ];
        };
        removed = {};
      };
      defaultApplications = {
        "application/pdf" = [ "emacs.desktop" ];
        "application/epub" = [ "emacs.desktop" ];
        "text/html" = [ "browser.desktop" ];
        "x-scheme-handler/https" = [ "browser.desktop" ];
        "x-scheme-handler/http" = [ "browser.desktop" ];
        "x-scheme-handler/tg" = [ "userapp-Telegram Desktop-1GLA50.desktop" ];
      };
    };

    userDirs = {
      enable = true;
      music = dir.music;
      videos = dir.videos;
      pictures = dir.pictures;
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
