{
  config,
  inputs,
  lib,
  pkgs,
  username,
  ...
}: {
  imports = [
    inputs.self.nixosModules.base
    inputs.self.nixosModules.home
    inputs.self.nixosModules.x
    inputs.self.nixosModules.zsa
  ];

  boot = {
    initrd = {
      availableKernelModules = ["ahci" "ehci_pci" "rtsx_pci_sdmmc" "sd_mod" "sr_mod" "usb_storage" "usbhid" "xhci_pci"];

      luks.devices.luks = {
        device = "/dev/disk/by-label/luks";
        preLVM = true;
        allowDiscards = true;
      };
    };

    kernelModules = ["dm-snapshot" "kvm-intel"];

    loader = {
      grub = {
        efiSupport = true;
        device = "nodev";
      };

      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  fileSystems = {
    "/" = {
      label = "root";
      fsType = "ext4";
    };
    "/boot" = {
      label = "boot";
      fsType = "vfat";
    };
  };

  hardware = {
    cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
    nvidia.package = config.boot.kernelPackages.nvidiaPackages.legacy_470;
    nvidia.powerManagement.enable = true;
  };

  networking = {
    firewall.allowedTCPPorts = [1337];
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
      nvidia.acceptLicense = true;
      permittedInsecurePackages = ["python-2.7.18.6" "nix-2.15.3"];
    };
    overlays = [
      # inputs.lem-flake.overlays.default
      inputs.addictions-tracker.overlays.default
      inputs.nur.overlays.default
      inputs.emacs-overlay.overlays.default
      inputs.self.overlays.default
    ];
    system = lib.mkDefault "x86_64-linux";
  };

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  swapDevices = [{device = "/dev/disk/by-label/swap";}];
  system.stateVersion = "22.05";
}
