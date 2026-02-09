{
  config,
  pkgs,
  inputs,
  ...
}: let
  inherit (config.colorScheme) palette;
in {
  imports = [
    inputs.nix-colors.homeManagerModule
    ./email.nix
    ./home
    ./programs
    ./services.nix
    ./xdg
    ./xresources.nix
  ];

  colorScheme = inputs.nix-colors.colorSchemes.gruvbox-light-medium;
  fonts.fontconfig.enable = true;

  gtk = {
    enable = true;
    font = {
      name = "Liberation Sans";
      size = 11;
    };
    gtk2.configLocation = "${config.xdg.cacheHome}/gtk-2.0/gtkrc";
    iconTheme = {
      name = "WhiteSur";
      package = pkgs.whitesur-icon-theme;
    };
    theme = {
      name = "WhiteSur";
      package = pkgs.whitesur-gtk-theme;
    };
  };

  qt = {
    enable = true;
    platformTheme.name = "gtk";
    style = {
      name = "gtk2";
      package = pkgs.libsForQt5.qtstyleplugins;
    };
  };

  systemd.user.services = {
    pueue = {
      Unit.Description = "Pueue Daemon - CLI process scheduler and manager";
      Service = {
        Restart = "no";
        ExecStart = "${pkgs.pueue}/bin/pueued";
        ExecReload = "${pkgs.pueue}/bin/pueued";
        Environment = "ASYNC_STD_THREAD_COUNT=4";
      };
      Install.WantedBy = ["default.target"];
    };

    transmission = {
      Unit = {
        Description = "Transmission BitTorrent Daemon";
        After = ["network.target"];
      };
      Service = {
        Type = "notify";
        ExecStart = "${pkgs.transmission_4}/bin/transmission-daemon -f --log-error";
        ExecReload = "${pkgs.util-linux}/bin/kill -s HUP $MAINPID";
        NoNewPrivileges = true;
      };
      Install.WantedBy = ["default.target"];
    };
  };

  xsession = {
    enable = true;
    initExtra = ''
      xset +fp ${pkgs.terminus_font}/share/fonts/terminus
    '';
    scriptPath = ".xinitrc";
    windowManager.command = ''
      eval "$(${pkgs.openssh}/bin/ssh-agent)"
      ${pkgs.stumpwm}/bin/stumpwm
      ${pkgs.openssh}/bin/ssh-agent -k
    '';
  };
}
