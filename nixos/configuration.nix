# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, username, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot = {
    loader = {
      grub = {
        efiSupport = true; # uefi
        # Define on which hard drive you want to install Grub.
        device = "nodev"; # uefi
      };
      systemd-boot.enable = true; # uefi
      efi.canTouchEfiVariables = true; # uefi
    };
    cleanTmpDir = true;
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak";
  };

  documentation.man.generateCaches = true;

  environment.systemPackages = with pkgs; [];

  hardware = {
    nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
    pulseaudio.enable = true;
  };

  i18n.defaultLocale = "en_US.UTF-8";

  networking = {
    firewall.allowedTCPPorts = [ 8080 8000 3306 ];
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
    hostName = "stribog";
    interfaces = {
      eno1.useDHCP = true;
      wlo1.useDHCP = true;
    };
    networkmanager.enable = true;
    useDHCP = false;
  };

  services = {
    locate = {
      enable = true;
      interval = "13:00";
      locate = pkgs.plocate;
      localuser = null;
    };

    xserver = {
      enable = true;
      layout = "dvorak,ru";
      xkbVariant = ",ruu";
      xkbOptions = "ctrl:swapcaps,grp:shifts_toggle";
      libinput.enable = true;
      displayManager = {
        startx.enable = true;
        defaultSession = "none";
      };
      videoDrivers = [ "modesetting" "nvidia" ];
    };

    # openssh = {
    #   enable = true;
    #   permitRootLogin = "yes";
    # };
  };

  security.sudo.configFile = ''
    %wheel ALL=(ALL) ALL
  '';

  sound.enable = true;

  system.stateVersion = "20.09";

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

  users = {
    users = {
      ${username} = {
        initialHashedPassword = "";
        isNormalUser = true;
        extraGroups = [ "video" "wheel" "networkmanager" "audio" ];
        openssh.authorizedKeys = {
          keys = [];
          keyFiles = [];
        };
      };
    };
  };

}
