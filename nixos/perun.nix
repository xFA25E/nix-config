{
  inputs,
  lib,
  pkgs,
  username,
  ...
}: {
  imports = [inputs.self.nixosModules.base];

  boot = {
    initrd.availableKernelModules = ["ahci" "ohci_pci" "ehci_pci" "pata_atiixp" "usb_storage" "sd_mod" "sr_mod"];

    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
    };
  };

  networking = {
    hostName = "perun";
    interfaces = {
      enp6s0.useDHCP = lib.mkDefault true;
      wlp2s0.useDHCP = lib.mkDefault true;
    };
  };

  nix.settings.trusted-users = [username];

  nixpkgs = {
    config.allowUnfree = true;
    overlays = [inputs.self.overlays.default];
    system = "x86_64-linux";
  };

  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    passwordAuthentication = false;
  };

  users.users.${username} = {
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDEbZ9Kz4oCbrybWc7jM6Oc7+UKFDsXtb/8IzmtpRb5flqXKy0ghUDLAQl/tur7du0HuX8la5Qsko/IbXN2ZK+2lqiWUnszAPA8P6DdLLO+U9W6yR5LqpIZLpDOwhQVf/IkrNEQXAGEP46YpYLLsn6SATQnXSy87Ri/au6+4joOMoQN9rjKPDD638BDDzFMf3fEbDotC1H5sBPHlrk09hsD4/pyrxmn7UJouT6cGWkuqXAx/NclGnay9hAsue00QqUYK62IC2wE9vNvmzWlAX2eVzo4envypNTe4tYDYS6BGklZP04AcgOwlVeOO+eUkPFMLgVSfJyNqpxXIugS05w9 vlr.ltkvsk@protonmail.com"
    ];
    packages = [pkgs.mahogany pkgs.seatd];
  };

  security.polkit.enable = true;
  hardware.opengl.enable = true;
  fonts.enableDefaultFonts = true;
  programs.dconf.enable = true;
  programs.xwayland.enable = true;
  xdg.portal.extraPortals = [pkgs.xdg-desktop-protal-wlr];
}
