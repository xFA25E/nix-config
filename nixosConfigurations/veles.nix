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
    inputs.self.nixosModules.home
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

  swapDevices = [{device = "/swap/swapfile";}];
  system.stateVersion = "24.11";
}
