{
  config,
  lib,
  modulesPath,
  pkgs,
  username,
  ...
}: {
  imports = [(modulesPath + "/installer/scan/not-detected.nix")];

  boot = {
    extraModulePackages = [];
    initrd = {
      availableKernelModules = ["nvme" "xhci_pci" "ahci" "usb_storage" "sd_mod"];
      kernelModules = [];
    };
    kernelModules = ["kvm-amd"];
    loader = {
      systemd-boot.enable = true;
      efi.canTouchEfiVariables = true;
    };
  };

  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak";
  };

  fileSystems = {
    "/" = {
      label = "nixos";
      fsType = "btrfs";
      options = ["compress=zstd" "subvol=root"];
    };

    "/home" = {
      label = "nixos";
      fsType = "btrfs";
      options = ["compress=zstd" "subvol=home"];
    };

    "/nix" = {
      label = "nixos";
      fsType = "btrfs";
      options = ["compress=zstd" "noatime" "subvol=nix"];
    };

    "/boot" = {
      label = "EFI";
      fsType = "vfat";
      options = ["fmask=0022" "dmask=0022"];
    };
  };

  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
  i18n.defaultLocale = "en_US.UTF-8";

  networking = {
    hostName = "svarog";
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
  };

  nix.settings.trusted-users = [username];
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";

  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "no";
    settings.PasswordAuthentication = false;
  };

  swapDevices = [];
  system.stateVersion = "24.11";
  time.timeZone = "Europe/Rome";

  users.users.${username} = {
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
    initialHashedPassword = "";
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDEbZ9Kz4oCbrybWc7jM6Oc7+UKFDsXtb/8IzmtpRb5flqXKy0ghUDLAQl/tur7du0HuX8la5Qsko/IbXN2ZK+2lqiWUnszAPA8P6DdLLO+U9W6yR5LqpIZLpDOwhQVf/IkrNEQXAGEP46YpYLLsn6SATQnXSy87Ri/au6+4joOMoQN9rjKPDD638BDDzFMf3fEbDotC1H5sBPHlrk09hsD4/pyrxmn7UJouT6cGWkuqXAx/NclGnay9hAsue00QqUYK62IC2wE9vNvmzWlAX2eVzo4envypNTe4tYDYS6BGklZP04AcgOwlVeOO+eUkPFMLgVSfJyNqpxXIugS05w9 vlr.ltkvsk@protonmail.com"
    ];
  };
}
