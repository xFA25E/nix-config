{
  description = "xFA25E various nix configurations";

  inputs = {
    agenix = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:ryantm/agenix";
    };

    emacs-overlay = {
      inputs = {
        nixpkgs-stable.follows = "nixpkgs";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:nix-community/emacs-overlay";
    };

    epkg-abbrev-hook = {
      inputs = {
        emacs-overlay.follows = "emacs-overlay";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:xFA25E/abbrev-hook";
    };

    epkg-amded = {
      inputs = {
        emacs-overlay.follows = "emacs-overlay";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:xFA25E/amded";
    };

    epkg-cyrillic-dvorak-im = {
      inputs = {
        emacs-overlay.follows = "emacs-overlay";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:xFA25E/cyrillic-dvorak-im";
    };

    epkg-dired-atool-transient = {
      inputs = {
        emacs-overlay.follows = "emacs-overlay";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:xFA25E/dired-atool-transient";
    };

    epkg-dired-tags = {
      inputs = {
        emacs-overlay.follows = "emacs-overlay";
        epkg-xattr.follows = "epkg-xattr";
        nixpkgs.follows = "nixpkgs";
      };

      url = "github:xFA25E/dired-tags";
    };

    epkg-pueue = {
      inputs = {
        emacs-overlay.follows = "emacs-overlay";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:xFA25E/pueue";
    };

    epkg-rx-widget = {
      inputs = {
        emacs-overlay.follows = "emacs-overlay";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:xFA25E/rx-widget";
    };

    epkg-sdcwoc = {
      inputs = {
        emacs-overlay.follows = "emacs-overlay";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:xFA25E/sdcwoc";
    };

    epkg-tempo-extra = {
      inputs = {
        emacs-overlay.follows = "emacs-overlay";
        epkg-abbrev-hook.follows = "epkg-abbrev-hook";
        nixpkgs.follows = "nixpkgs";
      };

      url = "github:xFA25E/tempo-extra";
    };

    epkg-xattr = {
      inputs = {
        emacs-overlay.follows = "emacs-overlay";
        nixpkgs.follows = "nixpkgs";
      };
      url = "github:xFA25E/xattr";
    };

    fu.url = "github:numtide/flake-utils";

    grobi = {
      flake = false;
      url = "github:fd0/grobi";
    };

    gmdb2 = {
      flake = false;
      url = "github:mdbtools/gmdb2";
    };

    home-manager = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/home-manager/release-25.05";
    };

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";

    nix-colors.url = "github:Misterio77/nix-colors";

    nixos-hardware.url = "github:NixOS/nixos-hardware/master";

    nur = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:nix-community/NUR";
    };

    sharry = {
      inputs.nixpkgs.follows = "nixpkgs";
      url = "github:eikek/sharry";
    };

    simple-nixos-mailserver = {
      inputs = {
        nixpkgs.follows = "nixpkgs";
        nixpkgs-25_05.follows = "nixpkgs";
      };
      url = "gitlab:simple-nixos-mailserver/nixos-mailserver/nixos-25.05";
    };

    stumpwm = {
      flake = false;
      url = "github:stumpwm/stumpwm/24.11";
    };
  };

  outputs = inputs: let
    system = inputs.fu.lib.system.x86_64-linux;
    pkgs = import inputs.nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays = [
        inputs.agenix.overlays.default
        inputs.nur.overlays.default
        inputs.emacs-overlay.overlays.default
        inputs.self.overlays.default

        inputs.sharry.overlays.default
      ];
    };
  in {
    nixosConfigurations = import ./nixosConfigurations inputs system pkgs;
    nixosModules = import ./nixosModules;
    homeModules = import ./homeModules;
    overlays = import ./overlays inputs;
    templates = import ./templates;

    apps.${system} = import ./apps.nix pkgs;
    devShells.${system}.default = pkgs.callPackage ./shell.nix {};
    packages.${system} = inputs.fu.lib.flattenTree (import ./packages.nix pkgs);
  };
}
