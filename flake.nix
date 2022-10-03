# TODOs
# write update scrits for wallpapers, stumpwm, stardicts
# write modules and write nixos
{
  description = "xFA25E various nix configurations";

  inputs = {
    amded.flake = false;
    amded.url = "github:ft/amded";

    discord.flake = false;
    discord.url = "https://dl.discordapp.net/apps/linux/0.0.20/discord-0.0.20.tar.gz";

    emacs-overlay.inputs.flake-utils.follows = "flake-utils";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    epkg-amded.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-amded.inputs.nixpkgs.follows = "nixpkgs";
    epkg-amded.url = "github:xFA25E/amded";

    epkg-cyrillic-dvorak-im.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-cyrillic-dvorak-im.inputs.nixpkgs.follows = "nixpkgs";
    epkg-cyrillic-dvorak-im.url = "github:xFA25E/cyrillic-dvorak-im";

    epkg-dired-tags.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-dired-tags.inputs.nixpkgs.follows = "nixpkgs";
    epkg-dired-tags.url = "github:xFA25E/dired-tags";

    epkg-flymake-statix.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-flymake-statix.inputs.nixpkgs.follows = "nixpkgs";
    epkg-flymake-statix.url = "github:xFA25E/flymake-statix";

    epkg-nixos-options.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-nixos-options.inputs.nixpkgs.follows = "nixpkgs";
    epkg-nixos-options.url = "github:xFA25E/nixos-options";

    epkg-rx-widget.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-rx-widget.inputs.nixpkgs.follows = "nixpkgs";
    epkg-rx-widget.url = "github:xFA25E/rx-widget";

    epkg-sdcwoc.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-sdcwoc.inputs.nixpkgs.follows = "nixpkgs";
    epkg-sdcwoc.url = "github:xFA25E/sdcwoc";

    epkg-skempo.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-skempo.inputs.nixpkgs.follows = "nixpkgs";
    epkg-skempo.url = "github:xFA25E/skempo";

    flake-utils.url = "github:numtide/flake-utils";

    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.utils.follows = "flake-utils";
    home-manager.url = "github:nix-community/home-manager/release-22.05";

    mpv-youtube-quality.flake = false;
    mpv-youtube-quality.url = "github:jgreco/mpv-youtube-quality";

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";

    nix-colors.url = "github:Misterio77/nix-colors";

    nur.url = "github:nix-community/NUR";

    stumpwm.flake = false;
    stumpwm.url = "github:stumpwm/stumpwm/22.05";

    unflac.flake = false;
    unflac.url = "git+https://git.sr.ht/~ft/unflac";
  };

  outputs = {
    flake-utils,
    nixpkgs,
    self,
    ...
  } @ inputs: let
    inherit (builtins) elem;
    inherit (flake-utils.lib) eachDefaultSystem flattenTree;
    inherit (nixpkgs.lib) nixosSystem;
    inherit (nixpkgs.lib.attrsets) filterAttrs mapAttrsToList;
    inherit (nixpkgs.lib.strings) hasPrefix;

    epkgFlakes = filterAttrs (name: _: hasPrefix "epkg-" name) inputs;

    system = "x86_64-linux";
    username = "val";
    pkgs = import nixpkgs {
      inherit system;
      config.allowUnfree = true;
      overlays =
        [inputs.nur.overlay inputs.emacs-overlay.overlay]
        ++ (mapAttrsToList (_: value: value.overlays.default) epkgFlakes)
        ++ [self.overlays.default];
    };
  in
    {
      nixosConfigurations = {
        stribog = nixosSystem {
          inherit pkgs system;
          modules = [
            self.nixosModules.nix
            ./nixos/stribog.nix
            inputs.home-manager.nixosModules.home-manager
            {
              home-manager = {
                extraSpecialArgs = {inherit (inputs) nix-colors;};
                useGlobalPkgs = true;
                useUserPackages = true;
                users.${username} = import ./home.nix;
              };
            }
          ];
          specialArgs = {inherit username;};
        };

        perun = nixosSystem {
          inherit pkgs system;
          modules = [self.nixosModules.nix ./nixos/perun.nix];
          specialArgs = {inherit username;};
        };
      };

      nixosModules.nix.config.nix = {
        registry = {
          nix-config.flake = self;
          nixpkgs.flake = nixpkgs;
        };
        settings.nix-path = ["nixpkgs=${nixpkgs}"];
      };

      overlays.default = import ./overlay inputs;
      templates = import ./templates;
    }
    // eachDefaultSystem (system: let
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [self.overlays.default];
      };
    in {
      apps = import ./apps.nix pkgs;
      devShells.default = pkgs.callPackage ./shell.nix {};
      packages = flattenTree (import ./packages.nix pkgs);
    });
}
