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

  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    pulse.enable = true;
    jack.enable = true;
    alsa = {
      enable = true;
      support32Bit = true;
    };

    extraConfig = {
      client."10-resample-quality"."context.properties"."resample.quality" = 4;
      pipewire."99-input-denoising"."context.modules" = [
        {
          "name" = "libpipewire-module-filter-chain";
          "args" = {
            "node.description" = "Noise Canceling source";
            "media.name" = "Noise Canceling source";
            "filter.graph" = {
              "nodes" = [
                {
                  "type" = "ladspa";
                  "name" = "rnnoise";
                  "plugin" = "${pkgs.rnnoise-plugin}/lib/ladspa/librnnoise_ladspa.so";
                  "label" = "noise_suppressor_mono";
                  "control" = {
                    "VAD Threshold (%)" = 50.0;
                    "VAD Grace Period (ms)" = 200;
                    "Retroactive VAD Grace (ms)" = 0;
                  };
                }
                # {
                #   "name" = "gain";
                #   "type" = "ladspa";
                #   "plugin" = "gain";
                #   "label" = "volume";
                #   "control" = {
                #     "Gain (dB)" = 10.0;
                #   };
                # }
              ];
            };
            "capture.props" = {
              "node.name" = "capture.rnnoise_source";
              "node.passive" = true;
              "audio.rate" = 48000;
            };
            "playback.props" = {
              "node.name" = "rnnoise_source";
              "media.class" = "Audio/Source";
              "audio.rate" = 48000;
            };
          };
        }
      ];
    };
  };

  environment.systemPackages = [pkgs.rnnoise pkgs.helvum pkgs.qpwgraph];

  virtualisation.docker.enable = true;
  virtualisation.docker.storageDriver = "btrfs";
  # virtualisation.docker.rootless = {
  #   enable = true;
  #   setSocketVariable = true;
  # };
}
