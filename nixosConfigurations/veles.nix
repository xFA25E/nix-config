{
  config,
  inputs,
  lib,
  modulesPath,
  pkgs,
  ...
}: {
  imports = [
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-t480
    inputs.self.nixosModules.base
    inputs.self.nixosModules.bluetooth
    inputs.self.nixosModules.desktop
    inputs.self.nixosModules.home
    inputs.self.nixosModules.pipewire
    inputs.self.nixosModules.x
    inputs.self.nixosModules.sshd
  ];

  boot = {
    initrd = {
      availableKernelModules = ["xhci_pci" "nvme" "usb_storage" "usbhid" "sd_mod"];
      kernelModules = [];
      luks.devices."enc".device = "/dev/disk/by-label/luks";
    };
    kernelModules = [];
    extraModulePackages = [];
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  fileSystems = {
    "/" = {
      label = "nixos";
      fsType = "btrfs";
      options = ["subvol=root" "compress=zstd"];
    };

    "/home" = {
      label = "nixos";
      fsType = "btrfs";
      options = ["subvol=home" "compress=zstd"];
    };

    "/nix" = {
      label = "nixos";
      fsType = "btrfs";
      options = ["subvol=nix" "compress=zstd" "noatime"];
    };

    "/boot" = {
      label = "ESP";
      fsType = "vfat";
      options = ["fmask=0022" "dmask=0022"];
    };
  };

  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  networking = {
    hostName = "veles";
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
  };

  networking.networkmanager.ensureProfiles.profiles = {
    "Wired Connection 1" = {
      connection.type = "ethernet";
      connection.id = "Wired Connection 1";
      connection.interface-name = "enp0s31f6"; # Make sure this matches your interface
      connection.autoconnect = true;

      ipv4.method = "manual";
      ipv4.addresses = "192.168.10.200/24";
      ipv4.gateway = "255.255.255.0";
      ipv4.dns = "8.8.8.8";
      ipv4.route-metric = 1000;
    };
  };

  swapDevices = [{device = "/swap/swapfile";}];
  system.stateVersion = "24.11";
}
