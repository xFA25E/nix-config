{
  config,
  inputs,
  lib,
  pkgs,
  username,
  ...
}: {
  imports = [
    inputs.home-manager.nixosModules.home-manager
    inputs.self.nixosModules.base
  ];

  boot = {
    initrd.availableKernelModules = ["xhci_pci" "ehci_pci" "ahci" "usb_storage" "usbhid" "sd_mod" "sr_mod" "rtsx_pci_sdmmc"];
    kernelModules = ["kvm-intel"];
    loader = {
      grub = {
        efiSupport = true;
        device = "nodev";
      };
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  hardware = {
    nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
    pulseaudio.enable = true;
  };

  home-manager = {
    extraSpecialArgs = {inherit inputs;};
    useGlobalPkgs = true;
    useUserPackages = true;
    users.${username} = import ./home;
  };

  networking = {
    firewall.allowedTCPPorts = [1337 8080 8000];
    hosts."10.233.1.2" = [
      "app.notifile.local"
      "api.notifile.local"
    ];
    hostName = "stribog";
    interfaces = {
      eno1.useDHCP = lib.mkDefault true;
      wlo1.useDHCP = lib.mkDefault true;
    };
  };

  nixpkgs = {
    config.allowUnfree = true;
    overlays = [
      inputs.nur.overlay
      inputs.emacs-overlay.overlay
      inputs.epkg-amded.overlays.default
      inputs.epkg-cyrillic-dvorak-im.overlays.default
      inputs.epkg-dired-tags.overlays.default
      inputs.epkg-flymake-statix.overlays.default
      inputs.epkg-nixos-options.overlays.default
      inputs.epkg-rx-widget.overlays.default
      inputs.epkg-sdcwoc.overlays.default
      inputs.epkg-skempo.overlays.default
      inputs.self.overlays.default
    ];
    system = "x86_64-linux";
  };

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  programs.dconf.enable = true;

  services = {
    i2pd = {
      # enable = true;
      proto = {
        i2cp.enable = true;
        httpProxy = {
          enable = true;
          # outproxy = "http://acetone.i2p:8888";
          # These are not working as expected
          inbound.length = 1;
          outbound.length = 1;
        };
      };
    };

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
      videoDrivers = ["modesetting" "nvidia"];
    };
  };

  sound.enable = true;
  users.users.${username}.extraGroups = ["video" "audio"];
}