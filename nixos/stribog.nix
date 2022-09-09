{
  config,
  pkgs,
  lib,
  username ? "val",
  ...
}: {
  imports = [(import ./common.nix)];

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

  networking = {
    firewall.allowedTCPPorts = [1337 8080 8000];
    hostName = "stribog";
    interfaces = {
      eno1.useDHCP = lib.mkDefault true;
      wlo1.useDHCP = lib.mkDefault true;
    };
  };

  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  services = {
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
