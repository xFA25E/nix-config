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
    keyboard.zsa.enable = true;
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
    config = {
      allowUnfree = true;
      nvidia.acceptLicense = true;
      permittedInsecurePackages = ["python-2.7.18.6" "nix-2.15.3"];
    };
    overlays = [
      # inputs.lem-flake.overlays.default
      inputs.addictions-tracker.overlays.default
      inputs.nur.overlay
      inputs.emacs-overlay.overlays.default
      inputs.self.overlays.default
    ];
    system = "x86_64-linux";
  };

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  programs.dconf.enable = true;

  services = {
    displayManager.defaultSession = "none";
    libinput.enable = true;

    locate = {
      enable = true;
      interval = "13:00";
      package = pkgs.plocate;
      localuser = null;
    };

    xserver = {
      enable = true;
      xkb = {
        layout = "us,ru";
        options = "ctrl:swapcaps,grp:shifts_toggle";
        variant = "dvorak,ruu";
      };
      displayManager.startx.enable = true;
      videoDrivers = ["modesetting" "nvidia"];
    };
  };

  sound.enable = true;
  users.users.${username}.extraGroups = ["video" "audio"];
}
