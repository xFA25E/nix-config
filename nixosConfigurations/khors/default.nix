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
    inputs.self.nixosModules.base
    inputs.self.nixosModules.sshd
  ];

  age.secrets."mail".file = ./secrets/mail.age;

  boot = {
    initrd = {
      availableKernelModules = ["ata_piix" "uhci_hcd" "xen_blkfront" "vmw_pvscsi"];
      kernelModules = ["nvme"];
    };
    loader.grub.device = "/dev/sda";
  };

  fileSystems."/" = {
    device = "/dev/sda2";
    fsType = "ext4";
  };

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
      "polimi@litkov.one" = {
        aliases = [];
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
    firewall.allowedTCPPorts = [17171];
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

  programs.git.enable = true;

  security.acme = {
    acceptTerms = true;
    defaults.email = "vlr.ltkvsk@protonmail.com";
  };

  services = {
    static-web-server = {
      enable = true;
      listen = "[::]:17171";
      root = "/home/${username}/static-web-server";
      configuration.general.directory-listing = true;
    };

    syncthing.enable = true;

    udev.extraRules = ''
      ATTR{address}=="52:54:82:2c:a4:90", NAME="enp3s0"
    '';
  };

  system.stateVersion = "22.11";
  zramSwap.enable = true;
}
