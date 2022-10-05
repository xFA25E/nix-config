{
  config,
  pkgs,
  inputs,
  ...
}: let
  inherit (config.colorScheme) colors;
in {
  imports = [
    inputs.nix-colors.homeManagerModule
    inputs.self.hmModules.firefoxSearchEngines
    ./email.nix
    ./home
    ./programs
    ./services.nix
    ./systemd.nix
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

  qt = {
    enable = true;
    platformTheme = "gtk";
    style = {
      name = "gtk2";
      package = pkgs.libsForQt5.qtstyleplugins;
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
      ${pkgs.dbus}/bin/dbus-run-session ${pkgs.stumpwm}/bin/stumpwm
      ${pkgs.openssh}/bin/ssh-agent -k
    '';
  };
}
