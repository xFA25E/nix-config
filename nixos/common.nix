{ config, pkgs, lib, modulesPath, username ? "val", ... }:

{
  imports = [ (modulesPath + "/installer/scan/not-detected.nix") ];

  boot.cleanTmpDir = true;

  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak";
  };

  documentation.man.generateCaches = true;

  fileSystems = {
    "/" = { device = "/dev/disk/by-label/root"; fsType = "ext4"; };
    "/boot" = { device = "/dev/disk/by-label/boot"; fsType = "vfat"; };
  };

  fonts = {
    enableGhostscriptFonts = true;
    fontDir.enable = true;
    fontconfig.defaultFonts.monospace = [ "Iosevka" ];
    fonts = with pkgs; [
      corefonts fira-code font-awesome hack-font hasklig inconsolata iosevka
      # nerdfonts
      open-sans source-code-pro unifont
    ];
  };

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  i18n.defaultLocale = "en_US.UTF-8";

  networking = {
    firewall.allowedTCPPorts = [ 8080 8000 ];
    hosts = {
      "0.0.0.0" = [
        "rewards.brave.com"
        "api.rewards.brave.com"
        "grant.rewards.brave.com"
        "variations.brave.com"
        "laptop-updates.brave.com"
        "static1.brave.com"
        "brave-core-ext.s3.brave.com"
      ];
    };
    hostFiles = [ "${pkgs.stevenblack-blocklist}/alternates/gambling-porn/hosts" ];
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
  };

  swapDevices = [ { device = "/dev/disk/by-label/swap"; } ];

  system.stateVersion = "22.05";

  systemd.services = {
    "loadkeys" = {
      enable = true;
      description = "Change caps to ctrl";
      wantedBy = [ "default.target" ];
      unitConfig = {
        Type = "oneshot";
      };
      serviceConfig = {
        ExecStart = "${pkgs.kbd}/bin/loadkeys ${./ctrl2caps.map}";
      };
    };
  };

  time.timeZone = "Europe/Rome";

  users.users.${username} = {
    initialHashedPassword = "";
    isNormalUser = true;
    extraGroups = [ "wheel" "networkmanager" ];
  };

}
