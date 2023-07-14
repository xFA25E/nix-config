{
  config,
  inputs,
  lib,
  modulesPath,
  username,
  ...
}: {
  imports = [
    inputs.agenix.nixosModules.default
    inputs.simple-nixos-mailserver.nixosModules.mailserver
    "${modulesPath}/profiles/qemu-guest.nix"
  ];

  age.secrets."mail".file = ./secrets/mail.age;

  boot = {
    initrd = {
      availableKernelModules = ["ata_piix" "uhci_hcd" "xen_blkfront" "vmw_pvscsi"];
      kernelModules = ["nvme"];
    };
    loader.grub.device = "/dev/sda";
    tmp.cleanOnBoot = true;
  };

  fileSystems."/" = {
    device = "/dev/sda2";
    fsType = "ext4";
  };

  i18n.defaultLocale = "en_US.UTF-8";

  mailserver = {
    certificateScheme = "acme-nginx";
    enable = true;
    fqdn = "mail.litkov.one";
    domains = ["litkov.one"];

    loginAccounts = {
      "valeriy@litkov.one" = {
        aliases = ["postmaster@litkov.one" "valery@litkov.one"];
        hashedPasswordFile = config.age.secrets."mail".path;
      };
    };

    mailboxes = {
      Archive = {
        auto = "subscribe";
        specialUse = "Archive";
      };
      Drafts = {
        auto = "subscribe";
        specialUse = "Drafts";
      };
      Flagged = {
        auto = "subscribe";
        specialUse = "Flagged";
      };
      Junk = {
        auto = "subscribe";
        specialUse = "Junk";
      };
      Sent = {
        auto = "subscribe";
        specialUse = "Sent";
      };
      Trash = {
        auto = "subscribe";
        specialUse = "Trash";
      };
    };
  };

  networking = {
    defaultGateway = "208.87.129.1";
    defaultGateway6 = {
      address = "2602:ff16:6::1";
      interface = "enp3s0";
    };
    dhcpcd.enable = false;
    domain = "";
    hostName = "khors";
    interfaces = {
      enp3s0 = {
        ipv4 = {
          addresses = [
            {
              address = "208.87.129.98";
              prefixLength = 24;
            }
          ];
          routes = [
            {
              address = "208.87.129.1";
              prefixLength = 32;
            }
          ];
        };
        ipv6 = {
          addresses = [
            {
              address = "2602:ff16:6:0:1:3a2:0:1";
              prefixLength = 64;
            }
            {
              address = "fe80::5054:82ff:fe2c:a490";
              prefixLength = 64;
            }
          ];
          routes = [
            {
              address = "2602:ff16:6::1";
              prefixLength = 128;
            }
          ];
        };
      };
    };
    nameservers = ["8.8.8.8"];
    usePredictableInterfaceNames = lib.mkForce true;
  };

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    registry.nixpkgs.flake = inputs.nixpkgs;
    settings = {
      auto-optimise-store = true;
      bash-prompt-suffix = ''$(printf '\10\10')nix \$ $(:)'';
      experimental-features = ["nix-command" "flakes"];
      max-jobs = "auto";
      nix-path = ["nixpkgs=${inputs.nixpkgs}"];
      trusted-users = [username];
    };
  };

  nixpkgs = {
    config.allowUnfree = true;
    overlays = [inputs.self.overlays.default];
    system = "x86_64-linux";
  };

  security.acme = {
    acceptTerms = true;
    defaults.email = "vlr.ltkvsk@protonmail.com";
  };

  services = {
    openssh = {
      enable = true;
      settings = {
        PermitRootLogin = "no";
        PasswordAuthentication = false;
      };
    };
    udev.extraRules = ''
      ATTR{address}=="52:54:82:2c:a4:90", NAME="enp3s0"
    '';
  };

  system.stateVersion = "22.11";

  time.timeZone = "Europe/Rome";

  users.users.${username} = {
    extraGroups = ["wheel"];
    initialHashedPassword = "";
    isNormalUser = true;
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDEbZ9Kz4oCbrybWc7jM6Oc7+UKFDsXtb/8IzmtpRb5flqXKy0ghUDLAQl/tur7du0HuX8la5Qsko/IbXN2ZK+2lqiWUnszAPA8P6DdLLO+U9W6yR5LqpIZLpDOwhQVf/IkrNEQXAGEP46YpYLLsn6SATQnXSy87Ri/au6+4joOMoQN9rjKPDD638BDDzFMf3fEbDotC1H5sBPHlrk09hsD4/pyrxmn7UJouT6cGWkuqXAx/NclGnay9hAsue00QqUYK62IC2wE9vNvmzWlAX2eVzo4envypNTe4tYDYS6BGklZP04AcgOwlVeOO+eUkPFMLgVSfJyNqpxXIugS05w9 vlr.ltkvsk@protonmail.com"
    ];
  };

  zramSwap.enable = true;
}
