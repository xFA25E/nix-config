# Search Engines:
# https://searx.be/?preferences=eJxtVcuO2zoM_ZrrjTFFH4uuvChaXNwBCkzRpN0KtEQrrCXRleRk3K8vnUSOMncWMSKaOjw8fFhDRsuRMHUWA0ZwjYNgZ7DYYXj4sWsca3DroYE5s2Y_OczYWWbrsCEvnmqK_Lx0-zhj4zEf2HTfnnb7JsGACSHqQ_e2yQf02HHSEJuIaXY5KQ4q4Ell6Lt_wSVsDJOSl-yOGDsGOb7haJvzrYeUFyHi2JJmg8fGUILeoVEYLAXJ4N2HDx-flTqSQU7_vP88kh4hJaX8nEiL4ciQlUqsCVzr0RCIcYFgUK5d8xeLJSvAkHJt1Fo_5GOFbik76JWiLIeIxpD4n_VY3yZEMyFGpQZyZ4ujPkJc2hUyUaqxBydM46u3t3A95X7WI-ZrxEsB2snB0no-SgUrZ7949CzBcoSQnBTZ1PES_gng7y1rmVZd2_VxjREWgJt6oG3iuYqSeVw4czrwCOHm57hPGd_EVFAmf_2XMsQ8rR1ThV7gwFwbeMIQceJ0007KTRDW25VIJxrJQIb67oXxxnCIiG3iIZ8gYmsoos4iy2sSruRvOWDkE91pNpjIq6VQOoCUc31cwYo6G70-G7L2dmGI4GFtgqLu75OkVIcoEIVEQdhI5cVzcNLrd8QihZFAV6FfFKYwmEOSXNOh8rzoVRy2gdkENMa2BgcKlInDXdPW6sE0pVdgttQhriWWWT9jzgbDXd7T2HqKkYuWL_jf2JDOf_g-fc-_EMfa0lOw9bnM5jX8u-f_5b8dKdTdg5nZpdcqVBIr0FXwa802kNsolzKWSy_20DqAdMT2fjVYSS6TxzJMVynLssZwvwKNrIj1ZyufbU9Obhav1P0k_-BoRHXgPOKysnySmVOftEYp25enR1nCp0gZ5c1jOFcfVdKRnSu-lxWupBnHbY_3MjRJMpV9njZmJejuvGFWV3355iwqoZN5FMQdukFJII4ezn0mtv_2-2-7isc-gmy_qH58_ypW2XgYG-kTFOi_m0eMQg==&q=%s
# https://duckduckgo.com/?kk=-1&kah=it-it&kl=wt-wt&ks=m&kaj=m&kam=osm&kp=-2&kn=-1&kd=1&kw=s&kak=-1&kax=-1&km=l&q=%s
# https://startpage.com/sp/search?query=hello&prfe=a715a36c09c1472e9d5d804b0ba9312716a96d474575edbfa5e7cb0c646b34216e65fa4ae420b5df58e6c8d3e420eb1771f23caa2663bb5435b01ebb741af66083a80b0bb3682e008b0e7e1126
{
  config,
  pkgs,
  lib,
  nix-colors,
  ...
}: let
  inherit (config.colorScheme) colors;
in {
  imports = [nix-colors.homeManagerModule];
  colorScheme = nix-colors.colorSchemes.gruvbox-light-medium;

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
      package = pkgs.nur.repos.rycee.materia-theme.override {
        configBase16 = {
          name = "Gruvbox-Light-Medium";
          kind = "light";
          colors = builtins.mapAttrs (_: color: {hex.rgb = color;}) colors;
        };
      };
    };
  };

  home = {
    extraOutputsToInstall = ["man" "doc" "info" "devdoc"];

    file = {
      ".mozilla/firefox/default/search.json.mozlz4_" = {
        onChange = ''
          cat ~/.mozilla/firefox/default/search.json.mozlz4_ > ~/.mozilla/firefox/default/search.json.mozlz4
        '';
        source = pkgs.runCommand "search.json.mozlz4" {
          nativeBuildInputs = [pkgs.mozlz4a];
          src = pkgs.writeText "search.json" ''
            {
              "engines" : [
                {
                  "_definedAliases" : [":yt"],
                  "_metaData" : {},
                  "_name" : "youtube",
                  "_urls" : [{
                    "params" : [],
                    "rels" : [],
                    "template" : "https://www.youtube.com/results?search_query={searchTerms}"
                  }]
                },
                {
                  "_definedAliases" : [":gh"],
                  "_metaData" : {},
                  "_name" : "github",
                  "_urls" : [{
                    "params" : [],
                    "rels" : [],
                    "template" : "https://github.com/search?q={searchTerms}"
                  }]
                },
                {
                  "_definedAliases" : [":sx"],
                  "_metaData" : {},
                  "_name" : "searx",
                  "_urls" : [{
                    "params" : [],
                    "rels" : [],
                    "template" : "https://searx.be/?preferences=eJxtVcuO2zoM_ZrrjTFFH4uuvChaXNwBCkzRpN0KtEQrrCXRleRk3K8vnUSOMncWMSKaOjw8fFhDRsuRMHUWA0ZwjYNgZ7DYYXj4sWsca3DroYE5s2Y_OczYWWbrsCEvnmqK_Lx0-zhj4zEf2HTfnnb7JsGACSHqQ_e2yQf02HHSEJuIaXY5KQ4q4Ell6Lt_wSVsDJOSl-yOGDsGOb7haJvzrYeUFyHi2JJmg8fGUILeoVEYLAXJ4N2HDx-flTqSQU7_vP88kh4hJaX8nEiL4ciQlUqsCVzr0RCIcYFgUK5d8xeLJSvAkHJt1Fo_5GOFbik76JWiLIeIxpD4n_VY3yZEMyFGpQZyZ4ujPkJc2hUyUaqxBydM46u3t3A95X7WI-ZrxEsB2snB0no-SgUrZ7949CzBcoSQnBTZ1PES_gng7y1rmVZd2_VxjREWgJt6oG3iuYqSeVw4czrwCOHm57hPGd_EVFAmf_2XMsQ8rR1ThV7gwFwbeMIQceJ0007KTRDW25VIJxrJQIb67oXxxnCIiG3iIZ8gYmsoos4iy2sSruRvOWDkE91pNpjIq6VQOoCUc31cwYo6G70-G7L2dmGI4GFtgqLu75OkVIcoEIVEQdhI5cVzcNLrd8QihZFAV6FfFKYwmEOSXNOh8rzoVRy2gdkENMa2BgcKlInDXdPW6sE0pVdgttQhriWWWT9jzgbDXd7T2HqKkYuWL_jf2JDOf_g-fc-_EMfa0lOw9bnM5jX8u-f_5b8dKdTdg5nZpdcqVBIr0FXwa802kNsolzKWSy_20DqAdMT2fjVYSS6TxzJMVynLssZwvwKNrIj1ZyufbU9Obhav1P0k_-BoRHXgPOKysnySmVOftEYp25enR1nCp0gZ5c1jOFcfVdKRnSu-lxWupBnHbY_3MjRJMpV9njZmJejuvGFWV3355iwqoZN5FMQdukFJII4ezn0mtv_2-2-7isc-gmy_qH58_ypW2XgYG-kTFOi_m0eMQg==&q={searchTerms}"
                  }]
                },
                {
                  "_definedAliases" : [":d"],
                  "_metaData" : {},
                  "_name" : "ddg",
                  "_urls" : [{
                    "params" : [],
                    "rels" : [],
                    "template" : "https://duckduckgo.com/?kk=-1&kah=it-it&kl=wt-wt&ks=m&kaj=m&kam=osm&kp=-2&kn=-1&kd=1&kw=s&kak=-1&kax=-1&km=l&q={searchTerms}"
                  }]
                },
                {
                  "_definedAliases" : [":sp"],
                  "_metaData" : {},
                  "_name" : "startpage",
                  "_urls" : [{
                    "params" : [],
                    "rels" : [],
                    "template" : "https://startpage.com/sp/search?query={searchTerms}&prfe=a715a36c09c1472e9d5d804b0ba9312716a96d474575edbfa5e7cb0c646b34216e65fa4ae420b5df58e6c8d3e420eb1771f23caa2663bb5435b01ebb741af66083a80b0bb3682e008b0e7e1126"
                  }]
                }
              ],
              "metaData" : {
                "appDefaultEngine" : "startpage",
                "current" : "startpage",
                "hash" : "wY9ZGT+vGpkZYQapRr7M4b0KjzBiyWf7TZuI+Rb7nxA="
              },
              "version" : 6
            }
          '';
        } "mozlz4a $src $out";
      };

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
      alsaUtils
      amded
      ascii
      bind
      binutils
      brave
      brave-incognito
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
      filename_put_duration
      format_seconds
      ghostscript
      gimp
      go-mtpfs
      hunspell
      hunspellDicts.en_US-large
      hunspellDicts.it_IT
      hunspellDicts.ru_RU
      image-dired-external-viewer
      image_clipboard
      imagemagick
      iw
      ledger
      leiningen
      libjpeg
      libnotify
      libreoffice
      make_backup
      mediainfo
      mkpasswd
      mpc_cli
      mpvi
      nload
      notifiers
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
      qrshow
      rar
      recode_video
      resize_video
      ripgrep
      rsync
      scrot
      sdcv
      shellcheck
      simplescreenrecorder
      sort_videos_by_duration
      speedtest-cli
      stalonetray
      strip_video
      stumpwm
      sxiv
      teams
      tor-browser-bundle-bin
      transmission
      unzip
      video_seconds
      wget
      woof
      xclip
      xdg-user-dirs
      xkb-switch
      xterm
      xz
      yt-dlp
      ytdl
      ytdla
      ytdlam
      ytdli
      ytdlp
      ytdlpa
      ytdlpam
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
      SUDO_ASKPASS = "${pkgs.sudo_askpass}/bin/sudo_askpass";
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
          flymake-eslint
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

    firefox = {
      enable = true;
      extensions = let
        inherit
          (pkgs.nur.repos.rycee.firefox-addons)
          buildFirefoxXpiAddon
          i-dont-care-about-cookies
          ublock-origin
          ;
        unhook = buildFirefoxXpiAddon {
          pname = "unhook";
          version = "1.6.1";
          addonId = "myallychou@gmail.com";
          url = "https://addons.mozilla.org/firefox/downloads/file/3936215/youtube_recommended_videos-1.6.1.xpi";
          sha256 = "sha256-BLP3B0AeiO/LXNJWBKjHGbL9zM82ysPx7VIaB0ZJTYE=";
          meta = {};
        };
      in [i-dont-care-about-cookies ublock-origin unhook];
      profiles = {
        default = {
          settings = {
            "accessibility.typeaheadfind" = false;
            "accessibility.typeaheadfind.flashBar" = 0;
            "app.normandy.first_run" = false;
            "app.shield.optoutstudies.enabled" = false;
            "app.update.auto" = false;
            "breakpad.reportURL" = "";
            "browser.bookmarks.restore_default_bookmarks" = false;
            "browser.discovery.enabled" = false;
            "browser.download.always_ask_before_handling_new_types" = true;
            "browser.download.dir" = "/tmp";
            "browser.download.folderList" = 2;
            "browser.download.panel.show" = true;
            "browser.download.panel.shown" = true;
            "browser.download.useDownloadDir" = false;
            "browser.download.viewableInternally.typeWasRegistered.avif" = true;
            "browser.download.viewableInternally.typeWasRegistered.webp" = true;
            "browser.formfill.enable" = false;
            "browser.laterrun.enabled" = true;
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.addons" = false;
            "browser.newtabpage.activity-stream.asrouter.userprefs.cfr.features" = false;
            "browser.newtabpage.activity-stream.feeds.section.topstories" = false;
            "browser.newtabpage.activity-stream.feeds.topsites" = false;
            "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.havePinned" = "";
            "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.searchEngines" = "";
            "browser.newtabpage.activity-stream.section.highlights.includeBookmarks" = false;
            "browser.newtabpage.activity-stream.section.highlights.includeDownloads" = false;
            "browser.newtabpage.activity-stream.section.highlights.includePocket" = false;
            "browser.newtabpage.activity-stream.section.highlights.includeVisited" = false;
            "browser.newtabpage.activity-stream.showSearch" = false;
            "browser.newtabpage.activity-stream.showSponsored" = false;
            "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;
            "browser.newtabpage.enabled" = false;
            "browser.newtabpage.pinned" = "[]";
            "browser.safebrowsing.downloads.enabled" = false;
            "browser.safebrowsing.malware.enabled" = false;
            "browser.safebrowsing.phishing.enabled" = false;
            "browser.search.hiddenOneOffs" = "Google,Amazon.com,Bing,DuckDuckGo,eBay,Wikipedia (en)";
            "browser.search.region" = "US";
            "browser.search.suggest.enabled" = false;
            "browser.shell.checkDefaultBrowser" = false;
            "browser.startup.homepage" = "about:blank";
            "browser.startup.homepage_override.mstone" = "ignore";
            "browser.tabs.crashReporting.sendReport" = false;
            "browser.toolbars.bookmarks.visibility" = "never";
            "browser.uiCustomization.state" = "{\"placements\":{\"widget-overflow-fixed-list\":[],\"nav-bar\":[\"back-button\",\"forward-button\",\"stop-reload-button\",\"urlbar-container\",\"downloads-button\",\"jid1-kkzogwgsw3ao4q_jetpack-browser-action\",\"ublock0_raymondhill_net-browser-action\",\"myallychou_gmail_com-browser-action\"],\"toolbar-menubar\":[\"menubar-items\"],\"TabsToolbar\":[\"tabbrowser-tabs\",\"new-tab-button\",\"alltabs-button\"],\"PersonalToolbar\":[\"import-button\",\"personal-bookmarks\"]},\"seen\":[\"save-to-pocket-button\",\"developer-button\",\"jid1-kkzogwgsw3ao4q_jetpack-browser-action\",\"ublock0_raymondhill_net-browser-action\",\"myallychou_gmail_com-browser-action\"],\"dirtyAreaCache\":[\"nav-bar\",\"PersonalToolbar\",\"toolbar-menubar\",\"TabsToolbar\"],\"currentVersion\":17,\"newElementCount\":3}";
            "browser.urlbar.placeholderName.private" = "";
            "browser.urlbar.quicksuggest.scenario" = "offline";
            "browser.urlbar.resultGroups" = "{\"children\":[{\"maxResultCount\":1,\"children\":[{\"group\":\"heuristicTest\"},{\"group\":\"heuristicExtension\"},{\"group\":\"heuristicSearchTip\"},{\"group\":\"heuristicOmnibox\"},{\"group\":\"heuristicEngineAlias\"},{\"group\":\"heuristicBookmarkKeyword\"},{\"group\":\"heuristicAutofill\"},{\"group\":\"heuristicPreloaded\"},{\"group\":\"heuristicTokenAliasEngine\"},{\"group\":\"heuristicFallback\"}]},{\"group\":\"extension\",\"availableSpan\":5},{\"flexChildren\":true,\"children\":[{\"group\":\"generalParent\",\"children\":[{\"availableSpan\":3,\"group\":\"inputHistory\"},{\"flexChildren\":true,\"children\":[{\"flex\":1,\"group\":\"remoteTab\"},{\"flex\":2,\"group\":\"general\"},{\"flex\":2,\"group\":\"aboutPages\"},{\"flex\":1,\"group\":\"preloaded\"}]},{\"group\":\"inputHistory\"}],\"flex\":2},{\"children\":[{\"flexChildren\":true,\"children\":[{\"flex\":2,\"group\":\"formHistory\"},{\"flex\":4,\"group\":\"remoteSuggestion\"}]},{\"group\":\"tailSuggestion\"}],\"flex\":1}]}]}";
            "browser.urlbar.shortcuts.bookmarks" = false;
            "browser.urlbar.shortcuts.history" = false;
            "browser.urlbar.shortcuts.tabs" = false;
            "browser.urlbar.showSearchSuggestionsFirst" = false;
            "browser.urlbar.suggest.bookmark" = false;
            "browser.urlbar.suggest.engines" = false;
            "browser.urlbar.suggest.history" = false;
            "browser.urlbar.suggest.openpage" = false;
            "browser.urlbar.suggest.quicksuggest.nonsponsored" = false;
            "browser.urlbar.suggest.quicksuggest.sponsored" = false;
            "browser.urlbar.suggest.searches" = false;
            "browser.urlbar.suggest.topsites" = false;
            "datareporting.healthreport.documentServerURI" = "";
            "datareporting.healthreport.service.enabled" = false;
            "datareporting.healthreport.uploadEnabled" = false;
            "datareporting.policy.dataSubmissionEnabled" = false;
            "doh-rollout.skipHeuristicsCheck" = true;
            "dom.battery.enabled" = false;
            "dom.event.clipboardevents.enabled" = false;
            "dom.forms.autocomplete.formautofill" = false;
            "dom.ipc.plugins.flash.subprocess.crashreporter.enabled" = false;
            "dom.ipc.plugins.reportCrashURL" = false;
            "extensions.formautofill.addresses.enabled" = false;
            "extensions.formautofill.creditCards.enabled" = false;
            "extensions.pictureinpicture.enable_picture_in_picture_overrides" = false;
            "extensions.pocket.enabled" = false;
            "extensions.screenshots.disabled" = true;
            "extensions.ui.dictionary.hidden" = true;
            "extensions.ui.locale.hidden" = true;
            "extensions.ui.sitepermission.hidden" = true;
            "extensions.webextensions.ExtensionStorageIDB.migrated.jid1-KKzOGWgsW3Ao4Q@jetpack" = true;
            "extensions.webextensions.ExtensionStorageIDB.migrated.uBlock0@raymondhill.net" = true;
            "extensions.webextensions.uuids" = "{\"uBlock0@raymondhill.net\":\"ed81545f-4149-47b9-a289-ea98eb20221d\",\"jid1-KKzOGWgsW3Ao4Q@jetpack\":\"38073834-2697-45b0-8238-c233089a7f8f\",\"doh-rollout@mozilla.org\":\"3e2f9e29-6753-4af0-87f4-30a67b9f1102\",\"formautofill@mozilla.org\":\"9b145aac-9bb4-4196-be4f-3cd25eefdb90\",\"pictureinpicture@mozilla.org\":\"4dc52942-8a8e-4aae-b312-6bbfd49aff0f\",\"screenshots@mozilla.org\":\"1deb33d4-528a-4034-9345-6c48cf23b301\",\"webcompat-reporter@mozilla.org\":\"dff469c9-d240-4a37-862f-a5b44fdf8b7c\",\"webcompat@mozilla.org\":\"82123520-5397-4573-9baa-f027fc401435\",\"default-theme@mozilla.org\":\"5296f9c7-d307-4aaf-b60a-406d4e0aa56f\",\"addons-search-detection@mozilla.com\":\"186e74f8-d5de-4409-a45a-55affa3faebb\",\"google@search.mozilla.org\":\"147e8d5f-207a-4397-8d05-11e68efdfd4a\",\"wikipedia@search.mozilla.org\":\"2faa6d11-eb7a-46bf-914f-663f5715b06e\",\"bing@search.mozilla.org\":\"fbc35289-8926-4380-ae6d-4b59984e28f8\",\"ddg@search.mozilla.org\":\"8624cf23-6a11-44c5-9143-26cb22b3fcc4\",\"amazon@search.mozilla.org\":\"86ed2a09-1db8-4add-bef7-b20dbea8621a\",\"amazondotcom@search.mozilla.org\":\"d4c17cbd-5aa0-4a57-885c-c484be3f138f\",\"myallychou@gmail.com\":\"4c2a7221-8620-40b5-9b25-572992d6e67f\",\"ebay@search.mozilla.org\":\"7016e5e1-271c-4e94-b611-ecbf00c5d9af\"}";
            "general.platform.override" = "Win32";
            "general.useragent.override" = "Mozilla/5.0 (Windows NT 10.0; rv:91.0) Gecko/20100101 Firefox/91.0";
            "geo.enabled" = false;
            "geo.wifi.logging.enabled" = false;
            "geo.wifi.uri" = "";
            "identity.fxaccounts.auth.uri" = "";
            "identity.fxaccounts.remote.force_auth.uri" = "";
            "identity.fxaccounts.remote.signin.uri" = "";
            "identity.fxaccounts.remote.signup.uri" = "";
            "identity.fxaccounts.settings.uri" = "";
            "layout.css.prefers-color-scheme.content-override" = 2;
            "layout.spellcheckDefault" = 0;
            "loop.enabled" = false;
            "media.autoplay.default" = 5;
            "media.hardwaremediakeys.enabled" = false;
            "media.navigator.enabled" = false;
            "media.videocontrols.picture-in-picture.video-toggle.enabled" = false;
            "network.captive-portal-service.enabled" = false;
            "network.dns.disablePrefetch" = true;
            "network.http.speculative-parallel-limit" = 0;
            "network.predictor.enabled" = false;
            "network.prefetch-next" = false;
            "pdfjs.enabledCache.state" = false;
            "permissions.default.desktop-notification" = 2;
            "permissions.default.geo" = 2;
            "permissions.default.xr" = 2;
            "places.history.enabled" = false;
            "pref.downloads.disable_button.edit_actions" = false;
            "privacy.clearOnShutdown.cache" = true;
            "privacy.clearOnShutdown.cookies" = true;
            "privacy.clearOnShutdown.downloads" = true;
            "privacy.clearOnShutdown.formdata" = true;
            "privacy.clearOnShutdown.history" = true;
            "privacy.clearOnShutdown.offlineApps" = true;
            "privacy.clearOnShutdown.openWindows" = true;
            "privacy.clearOnShutdown.passwords" = true;
            "privacy.clearOnShutdown.sessions" = true;
            "privacy.clearOnShutdown.siteSettings" = true;
            "privacy.donottrackheader.enabled" = true;
            "privacy.donottrackheader.value" = 1;
            "privacy.firstparty.isolate" = true;
            "privacy.history.custom" = true;
            "privacy.resistFingerprinting" = true; # might brake
            "privacy.sanitize.pending" = "[{\"id\":\"newtab-container\",\"itemsToClear\":[],\"options\":{}},{\"id\":\"shutdown\",\"itemsToClear\":[\"cache\",\"cookies\",\"offlineApps\",\"history\",\"formdata\",\"downloads\",\"sessions\",\"siteSettings\"],\"options\":{}}]";
            "privacy.sanitize.sanitizeOnShutdown" = true;
            "privacy.trackingprotection.enabled" = true;
            "services.sync.autoconnect" = false;
            "services.sync.clients.lastSync" = "0";
            "services.sync.declinedEngines" = "";
            "services.sync.engine.addons" = false;
            "services.sync.engine.addresses.available" = false;
            "services.sync.engine.bookmarks" = false;
            "services.sync.engine.history" = false;
            "services.sync.engine.passwords" = false;
            "services.sync.engine.prefs" = false;
            "services.sync.engine.tabs" = false;
            "services.sync.globalScore" = 0;
            "services.sync.nextSync" = 0;
            "services.sync.serverURL" = "";
            "services.sync.tabs.lastSync" = "0";
            "signon.autofillForms" = false;
            "signon.generation.enabled" = false;
            "signon.management.page.breach-alerts.enabled" = false;
            "signon.rememberSignons" = false;
            "toolkit.telemetry.archive.enabled" = false;
            "toolkit.telemetry.cachedClientID" = "";
            "toolkit.telemetry.enabled" = false;
            "toolkit.telemetry.optoutSample" = false;
            "toolkit.telemetry.pioneer-new-studies-available" = false;
            "toolkit.telemetry.prompted" = 2;
            "toolkit.telemetry.rejected" = true;
            "toolkit.telemetry.reportingpolicy.firstRun" = false;
            "toolkit.telemetry.server" = "";
            "toolkit.telemetry.unified" = false;
            "toolkit.telemetry.unifiedIsOptIn" = true;
            "webgl.disabled" = true;
            # "browser.privatebrowsing.autostart" = true;
            # "browser.uiCustomization.state" = "{\"placements\":{\"widget-overflow-fixed-list\":[],\"nav-bar\":[\"back-button\",\"forward-button\",\"stop-reload-button\",\"urlbar-container\",\"downloads-button\",\"jid1-kkzogwgsw3ao4q_jetpack-browser-action\",\"ublock0_raymondhill_net-browser-action\"],\"toolbar-menubar\":[\"menubar-items\"],\"TabsToolbar\":[\"tabbrowser-tabs\",\"new-tab-button\",\"alltabs-button\"],\"PersonalToolbar\":[\"import-button\",\"personal-bookmarks\"]},\"seen\":[\"save-to-pocket-button\",\"developer-button\",\"jid1-kkzogwgsw3ao4q_jetpack-browser-action\",\"ublock0_raymondhill_net-browser-action\"],\"dirtyAreaCache\":[\"nav-bar\",\"PersonalToolbar\",\"toolbar-menubar\",\"TabsToolbar\"],\"currentVersion\":17,\"newElementCount\":3}";
            # "privacy.sanitize.pending" = "[{\"id\":\"shutdown\",\"itemsToClear\":[\"cache\",\"cookies\",\"offlineApps\"],\"options\":{}},{\"id\":\"newtab-container\",\"itemsToClear\":[],\"options\":{}}]";
          };
          userChrome = "";
          userContent = "";
        };
      };
    };

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

  qt = {
    enable = true;
    platformTheme = "gtk";
    style = {
      name = "gtk2";
      package = pkgs.libsForQt5.qtstyleplugins;
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

    configFile = let
      inherit (pkgs.lib.strings) escapeShellArg;
      ebc = file: ''
        ${config.programs.emacs.finalPackage}/bin/emacs -Q --batch --eval \
          '(byte-compile-file (pop command-line-args-left))' \
          ${escapeShellArg "${config.xdg.configHome}/${file}"}
      '';
    in {
      "emacs/abbrev_defs".source = ./emacs/abbrev_defs;
      "emacs/custom.el".source = ./emacs/custom.el;
      "emacs/init.el" = {
        onChange = ebc "emacs/init.el";
        source = ./emacs/init.el;
      };
      "emacs/early-init.el" = {
        onChange = ebc "emacs/early-init.el";
        source = ./emacs/early-init.el;
      };
      "emacs/skempo/emacs-lisp.el" = {
        onChange = ebc "emacs/skempo/emacs-lisp.el";
        source = ./emacs/skempo/emacs-lisp.el;
      };
      "emacs/skempo/lisp.el" = {
        onChange = ebc "emacs/skempo/lisp.el";
        source = ./emacs/skempo/lisp.el;
      };
      "emacs/skempo/js.el" = {
        onChange = ebc "emacs/skempo/js.el";
        source = ./emacs/skempo/js.el;
      };
      "emacs/skempo/nix.el" = {
        onChange = ebc "emacs/skempo/nix.el";
        source = ./emacs/skempo/nix.el;
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
        (stumpwm:set-fg-color "#${base04}")
        (stumpwm:set-bg-color "#${base00}")
        (stumpwm:set-border-color "#${base03}")
        (stumpwm:set-focus-color "#${base04}")
        (stumpwm:set-unfocus-color "#${base00}")
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
    windowManager.command = ''
      eval "$(${pkgs.openssh}/bin/ssh-agent)"
      ${pkgs.dbus}/bin/dbus-run-session ${pkgs.stumpwm}/bin/stumpwm
      ${pkgs.openssh}/bin/ssh-agent -k
    '';
  };
}
