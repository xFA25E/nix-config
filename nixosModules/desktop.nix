{
  modulesPath,
  pkgs,
  ...
}: {
  imports = [(modulesPath + "/installer/scan/not-detected.nix")];

  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak";
  };

  documentation.man.generateCaches = true;

  fonts = {
    enableGhostscriptFonts = true;
    fontDir.enable = true;
    fontconfig.defaultFonts.monospace = ["Iosevka"];
    packages = with pkgs; [
      corefonts
      fira-code
      font-awesome
      hack-font
      hasklig
      inconsolata
      iosevka
      # nerdfonts
      open-sans
      source-code-pro
      unifont
    ];
  };

  networking.firewall = {
    allowedTCPPorts = [8080 8000 22000];
    allowedUDPPorts = [21027 22000];
  };

  services = {
    locate = {
      enable = true;
      interval = "13:00";
      package = pkgs.plocate;
    };
    udisks2.enable = true;
  };

  systemd.services = {
    "loadkeys" = {
      enable = true;
      description = "Change caps to ctrl";
      wantedBy = ["default.target"];
      unitConfig = {
        Type = "oneshot";
      };
      serviceConfig = {
        ExecStart = "${pkgs.kbd}/bin/loadkeys ${./ctrl2caps.map}";
      };
    };
  };

  programs.nix-ld.enable = true;
}
