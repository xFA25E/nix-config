{
  config,
  pkgs,
  ...
}: {
  imports = [
    ./bash.nix
    ./emacs.nix
    ./firefox.nix
    ./git.nix
    ./notmuch.nix
  ];

  programs = {
    direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };

    feh.enable = true;

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
      scripts = [pkgs.mpvScripts.quality-menu];
    };

    msmtp.enable = true;
    nix-index.enable = true;

    password-store = {
      enable = true;
      package = pkgs.pass.withExtensions (exts: [exts.pass-otp]);
      settings = {PASSWORD_STORE_DIR = "${config.xdg.dataHome}/password-store";};
    };

    readline = {
      enable = true;
      variables = {
        editing-mode = "emacs";
        blink-matching-paren = true;
      };
    };

    ssh = {
      enable = true;
      enableDefaultConfig = false;
      includes = ["config.d/config"];
      settings = {
        "*" = {
          AddKeysToAgent = "yes";
          ForwardAgent = false;
          StrictHostKeyChecking = "accept-new";
          HashKnownHosts = false;
          ServerAliveInterval = 15;
          ServerAliveCountMax = 4;
          ConnectTimeout = 15;
          ControlMaster = "auto";
          ControlPath = "\${XDG_RUNTIME_DIR}/ssh-cm-%C";
          ControlPersist = "10m";
        };
      };
    };
  };
}
