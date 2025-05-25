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

  swapDevices = [{device = "/swap/swapfile";}];
  system.stateVersion = "24.11";

  # services.pipewire.extraConfig.pipewire = {
  #   "99-custom" = {
  #     "context.properties" = {
  #       "default.clock.rate" = 44100;
  #       "default.clock.quantum" = 1024;
  #       "default.clock.min-quantum" = 32;
  #       "default.clock.max-quantum" = 2048;
  #     };
  #   };
  # };

  boot.kernelParams = ["usbcore.autosuspend=-1"];

  services.pipewire = {
    enable = true;
    pulse.enable = true;

    extraConfig.pipewire-pulse."99-rnnoise" = {
      "context.modules" = [
        {
          name = "libpipewire-module-echo-cancel";
          args = {
            # Tipo di cancellazione: "webrtc" (default) o "rnnoise"
            echo-cancel.source = "auto_source";
            echo-cancel.sink = "auto_null";
            use.hw-capture = false;
            use.hw-playback = false;
            # aec_method = "webrtc"; # oppure "rnnoise" se installato
            aec_method = "rnnoise"; # oppure "rnnoise" se installato
          };
        }
      ];
    };
  };

  environment.systemPackages = [pkgs.rnnoise pkgs.helvum];
}
