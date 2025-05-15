{
  config,
  inputs,
  lib,
  modulesPath,
  pkgs,
  username,
  ...
}: {
  imports = [
    inputs.nixos-hardware.nixosModules.common-cpu-intel-cpu-only
    inputs.nixos-hardware.nixosModules.common-gpu-nvidia-nonprime
    inputs.nixos-hardware.nixosModules.common-pc
    inputs.nixos-hardware.nixosModules.common-pc-ssd
    inputs.self.nixosModules.base
    inputs.self.nixosModules.bluetooth
    inputs.self.nixosModules.home
    inputs.self.nixosModules.x
    inputs.self.nixosModules.zsa
    inputs.self.nixosModules.nvidia
    inputs.self.nixosModules.sshd
  ];

  boot = {
    extraModulePackages = [];
    initrd = {
      availableKernelModules = ["ahci" "nvme" "sd_mod" "usb_storage" "xhci_pci"];
      kernelModules = [];
    };
    kernelModules = ["kvm-amd"];
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
      label = "EFI";
      fsType = "vfat";
      options = ["fmask=0022" "dmask=0022"];
    };
  };

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  networking = {
    hostName = "svarog";
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  swapDevices = [{device = "/swap/swapfile";}];
  system.stateVersion = "24.11";
}
