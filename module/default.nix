{
  config,
  inputs,
  lib,
  modulesPath,
  pkgs,
  username,
  ...
}: {
  imports = [(modulesPath + "/installer/scan/not-detected.nix")];

  boot = {
    cleanTmpDir = true;
    initrd = {
      kernelModules = ["dm-snapshot"];
      luks.devices.luks = {
        device = "/dev/disk/by-label/luks";
        preLVM = true;
        allowDiscards = true;
      };
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak";
  };

  documentation.man.generateCaches = true;

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/root";
      fsType = "ext4";
    };
    "/boot" = {
      device = "/dev/disk/by-label/boot";
      fsType = "vfat";
    };
  };

  fonts = {
    enableGhostscriptFonts = true;
    fontDir.enable = true;
    fontconfig.defaultFonts.monospace = ["Iosevka"];
    fonts = with pkgs; [
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

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  i18n.defaultLocale = "en_US.UTF-8";

  networking = {
    firewall.allowedTCPPorts = [8080 8000];
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
    hostFiles = ["${pkgs.stevenblack-blocklist}/alternates/gambling-porn/hosts"];
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
  };

  nix = {
    registry = {
      nix-config.flake = inputs.self;
      nixpkgs.flake = inputs.nixpkgs;
    };
    package = pkgs.nix;
    settings = {
      bash-prompt-suffix = ''$(printf '\10\10')nix \$ $(:)'';
      experimental-features = ["nix-command" "flakes"];
      keep-derivations = true;
      keep-outputs = true;
      max-jobs = "auto";
      nix-path = ["nixpkgs=${inputs.nixpkgs}"];
    };
  };

  programs.bash.promptInit = ''
    PS1='\n$(e=$?;[[ $e != 0 ]]&&printf "%s " "$e")\u $(p=''${PWD#"$HOME"};[[ $PWD != "$p" ]]&&printf "~";IFS=/;for q in ''${p:1};do printf "/%s" "''${q:0:1}";[[ ''${q:0:1} = . ]]&&printf "%s" "''${q:1:1}";done;printf "%s" "''${q:1}") \$ '
  '';

  swapDevices = [{device = "/dev/disk/by-label/swap";}];

  system.stateVersion = "22.05";

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

  time.timeZone = "Europe/Rome";

  users.users.${username} = {
    initialHashedPassword = "";
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };
}
