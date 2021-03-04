{ config, pkgs, ... }:
let
  colors = {
    base00 = "#FFFFFF";
    base01 = "#E0E0E0";
    base02 = "#D0D0D0";
    base03 = "#B0B0B0";
    base04 = "#000000";
    base05 = "#101010";
    base06 = "#151515";
    base07 = "#202020";
    base08 = "#FF0086";
    base09 = "#FD8900";
    base0A = "#ABA800";
    base0B = "#00C918";
    base0C = "#1FAAAA";
    base0D = "#3777E6";
    base0E = "#AD00A1";
    base0F = "#CC6633";
  };
in {
  home.packages = [
    pkgs.htop
  ];

  programs = {
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

    qutebrowser = {
      enable = true;
      package = pkgs.writeShellScriptBin "fuckthisshit" ''
        echo fuckthisshit
      '';
      aliases = {
        "emacs_source" = "spawn --userscript view_source";
      };
      keyBindings = {
        normal = {
          ",w" = "spawn --detach mpvi {url}";
          ",W" = "hint links spawn --detach mpvi {hint-url}";
          ",d" = "spawn --detach ytdli {url}";
          ",D" = "hint links spawn --detach ytdli {hint-url}";
          ",t" = "hint links spawn transmission-remote --add {hint-url}";
          "tjt" = "set content.javascript.enabled true";
          "tjf" = "set content.javascript.enabled false";
          "ge" = "emacs_source";
          "!" = "set-cmd-text :open !";
          "<Alt+!>" = "set-cmd-text :open -t !";
        };
      };
      searchEngines = {
        DEFAULT = "https://ddg.co/lite/?q={}";
        d = "https://ddg.co/?q={}&kk=-1&kah=it-it&kl=wt-wt&ks=m&kaj=m&kam=osm&kp=-2&kn=-1&kd=1&kw=s&kak=-1&kax=-1&km=l";
        yt = "https://www.youtube.com/results?search_query={}";
        bc = "https://www.bitchute.com/search/?query={}";
        rt = "https://rutracker.net/forum/tracker.php?nm={}";
        gtr = "https://translate.google.com/#view=home&op=translate&sl=auto&tl=ru&text={}";
        gte = "https://translate.google.com/#view=home&op=translate&sl=auto&tl=en&text={}";
        gti = "https://translate.google.com/#view=home&op=translate&sl=auto&tl=it&text={}";
        sp = "https://www.startpage.com/do/asearch?query={}&prf=4c27b3baec77a46344631f4164a09662";
        sx = "https://searx.be/?preferences=eJxtVcuO2zoM_ZrrjTFFH4uuvChaXNwBCkzRpN0KtEQrrCXRleRk3K8vnUSOMncWMSKaOjw8fFhDRsuRMHUWA0ZwjYNgZ7DYYXj4sWsca3DroYE5s2Y_OczYWWbrsCEvnmqK_Lx0-zhj4zEf2HTfnnb7JsGACSHqQ_e2yQf02HHSEJuIaXY5KQ4q4Ell6Lt_wSVsDJOSl-yOGDsGOb7haJvzrYeUFyHi2JJmg8fGUILeoVEYLAXJ4N2HDx-flTqSQU7_vP88kh4hJaX8nEiL4ciQlUqsCVzr0RCIcYFgUK5d8xeLJSvAkHJt1Fo_5GOFbik76JWiLIeIxpD4n_VY3yZEMyFGpQZyZ4ujPkJc2hUyUaqxBydM46u3t3A95X7WI-ZrxEsB2snB0no-SgUrZ7949CzBcoSQnBTZ1PES_gng7y1rmVZd2_VxjREWgJt6oG3iuYqSeVw4czrwCOHm57hPGd_EVFAmf_2XMsQ8rR1ThV7gwFwbeMIQceJ0007KTRDW25VIJxrJQIb67oXxxnCIiG3iIZ8gYmsoos4iy2sSruRvOWDkE91pNpjIq6VQOoCUc31cwYo6G70-G7L2dmGI4GFtgqLu75OkVIcoEIVEQdhI5cVzcNLrd8QihZFAV6FfFKYwmEOSXNOh8rzoVRy2gdkENMa2BgcKlInDXdPW6sE0pVdgttQhriWWWT9jzgbDXd7T2HqKkYuWL_jf2JDOf_g-fc-_EMfa0lOw9bnM5jX8u-f_5b8dKdTdg5nZpdcqVBIr0FXwa802kNsolzKWSy_20DqAdMT2fjVYSS6TxzJMVynLssZwvwKNrIj1ZyufbU9Obhav1P0k_-BoRHXgPOKysnySmVOftEYp25enR1nCp0gZ5c1jOFcfVdKRnSu-lxWupBnHbY_3MjRJMpV9njZmJejuvGFWV3355iwqoZN5FMQdukFJII4ezn0mtv_2-2-7isc-gmy_qH58_ypW2XgYG-kTFOi_m0eMQg==&q={}";
        btd = "https://www.btdig.com/search?q={}";
        sc = "https://soundcloud.com/search?q={}";
        gth = "https://github.com/search?utf8=%E2%9C%93&q={}&type=";
      };
      settings = {
        auto_save = {
          interval = 2147483647;
          session = false;
        };
        backend = "webengine";
        colors = {
          completion = {
            category = {
              bg = colors.base00;
              fg = colors.base0F;
              border = {
                bottom = colors.base0F;
                top = colors.base0F;
              };
            };
            even.bg = colors.base00;
            odd.bg = colors.base00;
            fg = [ colors.base04 colors.base0B colors.base08 ];
            item = {
              selected = {
                bg = colors.base00;
                fg = colors.base0C;
                border = {
                  bottom = colors.base0B;
                  top = colors.base0C;
                };
              };
            };
            match.fg = colors.base08;
            scrollbar = {
              bg = colors.base00;
              fg = colors.base04;
            };
          };
          downloads = {
            bar.bg = colors.base00;
            error = {
              bg = colors.base08;
              fg = colors.base00;
            };
            start = {
              bg = colors.base0D;
              fg = colors.base00;
            };
            stop = {
              bg = colors.base0B;
              fg = colors.base00;
            };
            system = {
              bg = "rgb";
              fg = "rgb";
            };
          };
          hints = {
            bg = colors.base00;
            fg = colors.base0B;
            match.fg = colors.base00;
          };
          keyhint = {
            bg = colors.base00;
            fg = colors.base07;
            suffix.fg = colors.base0A;
          };
          messages = {
            error = {
              bg = colors.base00;
              border = colors.base08;
              fg = colors.base08;
            };
            info = {
              bg = colors.base00;
              border = colors.base0B;
              fg = colors.base0B;
            };
            warning = {
              bg = colors.base00;
              border = colors.base09;
              fg = colors.base09;
            };
          };
          prompts = {
            bg = colors.base00;
            border = colors.base07;
            fg = colors.base0F;
            selected = {
              bg = colors.base0D;
            };
          };
          statusbar = {
            caret = {
              bg = colors.base00;
              fg = colors.base0E;
              selection = {
                bg = colors.base00;
                fg = colors.base0D;
              };
            };
            command = {
              bg = colors.base0C;
              fg = colors.base00;
              private = {
                bg = colors.base00;
                fg = colors.base0C;
              };
            };
            insert = {
              bg = colors.base00;
              fg = colors.base0B;
            };
            normal = {
              bg = colors.base00;
              fg = colors.base0E;
            };
            private = {
              bg = colors.base00;
              fg = colors.base0E;
            };
            progress.bg = colors.base04;
            url = {
              error.fg = colors.base08;
              fg = colors.base04;
              hover.fg = colors.base0D;
              success = {
                http.fg = colors.base07;
                https.fg = colors.base09;
              };
              warn.fg = colors.base0A;
            };
          };
          tabs = {
            bar.bg = colors.base02;
            even = {
              bg = colors.base0E;
              fg = colors.base00;
            };
            odd = {
              bg = colors.base0E;
              fg = colors.base00;
            };
            selected = {
              even = {
                bg = colors.base00;
                fg = colors.base0E;
              };
              odd = {
                bg = colors.base00;
                fg = colors.base0E;
              };
            };
            indicator = {
              system = "rgb";
              error = colors.base08;
              start = colors.base0D;
              stop = colors.base0B;
            };
          };
          webpage.bg = "#ffffff";
        };
        completion = {
          cmd_history_max_items = 0;
          height = "50%";
          quick = true;
          scrollbar = {
            padding = 0;
            width = 0;
          };
          show = "always";
          shrink = true;
          timestamp_format = "%Y-%m-%d";
          web_history.max_items = 0;
        };
        content = {
          autoplay = false;
          canvas_reading = false;
          cookies = {
            accept = "no-3rdparty";
            store = false;
          };
          default_encoding = "iso-8859-1";
          desktop_capture = "ask";
          dns_prefetch = true;
          geolocation = false;
          headers = {
            accept_language = "en-US,en;q=0.5";
            do_not_track = true;
            referer = "same-domain";
            user_agent = "Mozilla/5.0 (Windows NT 6.1; rv:52.0) Gecko/20100101 Firefox/52.0";
          };
          blocking = {
            enabled = true;
            hosts.lists = [
              "https://www.malwaredomainlist.com/hostslist/hosts.txt"
              "http://someonewhocares.org/hosts/hosts"
              "http://winhelp2002.mvps.org/hosts.zip"
              "http://malwaredomains.lehigh.edu/files/justdomains.zip"
              "https://pgl.yoyo.org/adservers/serverlist.php?hostformat=hosts&mimetype=plaintext"
            ];
          };
          hyperlink_auditing = false;
          images = true;
          javascript = {
            alert = true;
            can_access_clipboard = false;
            can_open_tabs_automatically = false;
            enabled = false;
            modal_dialog = false;
            prompt = true;
          };
          local_content_can_access_file_urls = true;
          local_content_can_access_remote_urls = false;
          local_storage = false;
          mouse_lock = "ask";
          mute = false;
          pdfjs = false;
          persistent_storage = "ask";
          register_protocol_handler = "ask";
          plugins = false;
          print_element_backgrounds = true;
          private_browsing = true;
          proxy = "system";
          ssl_strict = "ask";
          user_stylesheets = [
            (let youtubeCss = pkgs.writeText "youtube.css" ''
              html>body>ytd-app>div#content>ytd-page-manager#page-manager>ytd-watch-flexy>div#columns>div#secondary,
              html>body>ytd-app>div#content>ytd-page-manager#page-manager>ytd-watch-flexy>div#columns>div#primary>div#primary-inner>div#related,
              html>body>ytd-app>div#content.style-scope.ytd-app>div#masthead-container.style-scope.ytd-app,
              html>body>ytd-app>div#content.style-scope.ytd-app>app-drawer#guide.style-scope.ytd-app,
              html>body>ytd-app>div#content>ytd-page-manager#page-manager>ytd-browse>ytd-two-column-browse-results-renderer>div#primary>ytd-rich-grid-renderer {
                  display: none;
              }
            ''; in "${youtubeCss}")
          ];
          webgl = false;
          webrtc_ip_handling_policy = "default-public-interface-only";
          xss_auditing = false;
        };
        downloads = {
          location = {
            prompt = true;
            remember = true;
            suggestion = "path";
          };
          position = "top";
          remove_finished = 200;
        };
        editor = {
          command = [ "emacseditor" "{file}" "+{line}:{column}" ];
          encoding = "utf-8";
        };
        fonts = let defaultFont = "15pt default_family"; in {
          default_family = [ "Iosevka" "lucy tewi" ];
          completion = {
            entry = defaultFont;
            category = "bold " + defaultFont;
          };
          debug_console = defaultFont;
          downloads = defaultFont;
          hints = "bold 18pt default_family";
          keyhint = defaultFont;
          messages = {
            error = defaultFont;
            info = defaultFont;
            warning = defaultFont;
          };
          prompts = defaultFont;
          statusbar = defaultFont;
          tabs = {
            selected = "bold " + defaultFont;
            unselected = defaultFont;
          };
          web.size = {
            default = 16;
            default_fixed = 13;
            minimum = 0;
            minimum_logical = 6;
          };
        };
        hints = {
          auto_follow = "unique-match";
          auto_follow_timeout = 0;
          border = "1px solid " + colors.base07;
          chars = "aoeuhtns";
          dictionary = "/usr/share/dict/cracklib-small";
          hide_unmatched_rapid_hints = true;
          min_chars = 1;
          mode = "letter";
          scatter = true;
          uppercase = false;
        };
        history_gap_interval = -1;
        input = {
          forward_unbound_keys = "auto";
          insert_mode = {
            auto_leave = true;
            auto_load = false;
            plugins = false;
          };
          links_included_in_focus_chain = true;
          partial_timeout = 1000;
          mouse.rocker_gestures = false;
          spatial_navigation = false;
        };
        keyhint = {
          blacklist = [];
          delay = 2000;
        };
        messages.timeout = 1500;
        new_instance_open_target = "tab-silent";
        new_instance_open_target_window = "last-focused";
        prompt = {
          filebrowser = true;
          radius = 8;
        };
        qt = {
          low_end_device_mode = "auto";
          process_model = "process-per-site-instance";
          force_software_rendering = "none";
        };
        scrolling = {
          bar = "never";
          smooth = false;
        };
        search.ignore_case = "smart";
        session = {
          default_name = "default";
          lazy_restore = true;
        };
        spellcheck.languages = [ "en-US" "ru-RU" "it-IT" ];
        statusbar = {
          show = "always";
          position = "bottom";
          widgets = [ "keypress" "url" ];
        };
        tabs = {
          background = false;
          close_mouse_button = "none";
          favicons = {
            scale = 1.0;
            show = "always";
          };
          last_close = "close";
          max_width = -1;
          mousewheel_switching = false;
          new_position = {
            related = "next";
            unrelated = "last";
          };
          position = "top";
          select_on_remove = "last-used";
          show = "switching";
          show_switching_delay = 2000;
          tabs_are_windows = false;
          title = {
            alignment = "center";
            format = "{index}{audio} {current_title}";
            format_pinned = "{index}";
          };
          width = "20%";
          indicator.width = 0;
          wrap = true;
        };
        url = {
          auto_search = "naive";
          default_page = "about:blank";
          open_base_url = true;
          start_pages = "about:blank";
        };
        window = {
          hide_decoration = false;
          title_format = "{perc}({scroll_pos}){audio} {current_title}{title_sep}{current_url}";
        };
        zoom = {
          default = "150%";
          levels = [
            "25%" "33%" "39%" "50%" "67%" "75%" "90%" "100%" "110%" "125%"
            "150%" "175%" "200%" "250%" "300%" "400%" "500%"
          ];
          mouse_divider = 512;
        };
      };
      extraConfig = ''
        c.content.headers.custom = { "accept" : "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8" }
        for key in ["error", "info", "unknown", "warning"]:
            c.content.javascript.log[key] = "debug"
        for key in ["top", "bottom", "left", "right"]:
            c.statusbar.padding[key] = 0
            c.tabs.indicator.padding[key] = 0
            c.tabs.padding[key] = 0
        for site in [
                "*://vk.com/*", "*://www.youtube.com/*", "*://www.trenord.it/*",
                "*://duckduckgo.com/*", "*://klava.org/*", "*://soundcloud.com/*",
                "*://rutracker.net/*", "*://translate.google.com/*",
                "*://doc.rust-lang.org/*", "*://crates.io/*",
                "*://mail.protonmail.com/*", "*://github.com/*", "*://bitchute.com/*",
                "*://www.bitchute.com/*"
        ]:
            config.set('content.javascript.enabled', True, site)

        config.load_autoconfig(False)
      '';
    };
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
  };

  xresources.properties = {
    "xterm*cursorBlink" = "false";
    "xterm*font" = "xft:Hack:size=15:antialias=true:hinting=true";
    "xterm*internalBorder" = 0;
    "xterm*saveLines" = 10240;
    "xterm*scrollTtyOutput" = "false";
    "xterm*termName" = "xterm-color";
    "xterm*eightBitInput" = "false";
    "xterm*foreground" = colors.base05;
    "xterm*background" = colors.base00;
    "xterm*cursorColor" = colors.base08;
    "xterm*color0" = colors.base00;
    "xterm*color1" = colors.base08;
    "xterm*color2" = colors.base0B;
    "xterm*color3" = colors.base0A;
    "xterm*color4" = colors.base0D;
    "xterm*color5" = colors.base0E;
    "xterm*color6" = colors.base0C;
    "xterm*color7" = colors.base05;
    "xterm*color8" = colors.base03;
    "xterm*color9" = colors.base08;
    "xterm*color10" = colors.base0B;
    "xterm*color11" = colors.base0A;
    "xterm*color12" = colors.base0D;
    "xterm*color13" = colors.base0E;
    "xterm*color14" = colors.base0C;
    "xterm*color15" = colors.base07;
    "xterm*color16" = colors.base09;
    "xterm*color17" = colors.base0F;
    "xterm*color18" = colors.base01;
    "xterm*color19" = colors.base02;
    "xterm*color20" = colors.base04;
    "xterm*color21" = colors.base06;
    "Sxiv.foreground" = colors.base05;
    "Sxiv.background" = colors.base00;
    "Sxiv.font" = "Iosevka-15";
    "emacs.ScrollBars" = false;
    "emacs.ScrollBar" = false;
    "emacs.ToolBar" = false;
    "emacs.MenuBar" = false;
  };

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
  };

  xdg.dataFile = {
    "stardict/dic".source = "${pkgs.stardictDicts}/share/stardict/dic";
    "applications/emacsdired.desktop".source = ./applications/emacsdired.desktop;
    "applications/emacsmail.desktop".source = ./applications/emacsmail.desktop;
  };

  # programs.emacs = {
  #   enable = true;
  #   extraPackages = epkgs: with epkgs; [
  #     bui

  #     ace-link acme-theme apache-mode async avy bash-completion bicycle cargo
  #     cider clipmon clojure-mode consult diff-hl diminish dired-rsync dumb-jump
  #     edit-indirect eglot embark emmet-mode fd-dired flycheck
  #     flycheck-checkbashisms form-feed format-all free-keys gcmh geiser
  #     gitconfig-mode gitignore-mode htmlize insert-char-preview ipretty
  #     json-mode leaf ledger-mode magit marginalia mingus mu4e-alert
  #     neato-graph-bar nix-mode nov orderless org-mime outline-minor-faces
  #     pdf-tools php-mode quelpa restclient reverse-im rg robots-txt-mode
  #     rust-mode sdcv shr-tag-pre-highlight sly sly-asdf sly-quicklisp
  #     smartparens sqlup-mode sudo-edit transmission vlf web-mode wgrep which-key

  #     (csv-mode.override (oldAttrs: {
  #       elpaBuild = (attrs: oldAttrs.elpaBuild (attrs // {
  #         version = "1.15";
  #         src = oldAttrs.fetchurl {
  #           url = "https://elpa.gnu.org/packages/csv-mode-1.15.tar";
  #           sha256 = "0pigqhqg5mfza6jdskcr9yvrzdxnd68iyp3vyb8p8wskdacmbiyx";
  #         };
  #       }));
  #     }))

  #     dired-git-info modus-operandi-theme rainbow-mode sql-indent

  #     org-plus-contrib
  #   ];
  # };

  # programs.firefox = {
  #   enable = true;
  #   profiles = {
  #     myprofile = {
  #       settings = {
  #         "general.smoothScroll" = false;
  #       };
  #     };
  #   };
  # };

  # services.gpg-agent = {
  #   enable = true;
  #   defaultCacheTtl = 1800;
  #   enableSshSupport = true;
  # };

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
