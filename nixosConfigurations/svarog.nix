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
    inputs.self.nixosModules.base
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

  hardware = {
    cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
    bluetooth.enable = true;
  };

  networking = {
    hostName = "svarog";
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
  };

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  services.blueman.enable = true;
  swapDevices = [{device = "/swap/swapfile";}];
  system.stateVersion = "24.11";
}
