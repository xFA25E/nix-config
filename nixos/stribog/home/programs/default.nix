{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./bash.nix
    ./emacs.nix
    ./firefox.nix
    ./notmuch.nix
  ];

  programs = {
    direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
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
}
