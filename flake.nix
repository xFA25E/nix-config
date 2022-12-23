{
  description = "xFA25E various nix configurations";

  inputs = {
    amded.flake = false;
    amded.url = "github:ft/amded";

    discord.flake = false;
    discord.url = "https://dl.discordapp.net/apps/linux/0.0.22/discord-0.0.22.tar.gz";

    emacs-overlay.inputs.flake-utils.follows = "fu";
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

    epkg-nixos-options.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-nixos-options.inputs.nixpkgs.follows = "nixpkgs";
    epkg-nixos-options.url = "github:xFA25E/nixos-options";

    epkg-rx-widget.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-rx-widget.inputs.nixpkgs.follows = "nixpkgs";
    epkg-rx-widget.url = "github:xFA25E/rx-widget";

    epkg-sdcwoc.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-sdcwoc.inputs.nixpkgs.follows = "nixpkgs";
    epkg-sdcwoc.url = "github:xFA25E/sdcwoc";

    epkg-tempo-abbrev.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-tempo-abbrev.inputs.nixpkgs.follows = "nixpkgs";
    epkg-tempo-abbrev.url = "github:xFA25E/tempo-abbrev";

    fu.url = "github:numtide/flake-utils";

    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.inputs.utils.follows = "fu";
    home-manager.url = "github:nix-community/home-manager/release-22.05";

    mozlz4.flake = false;
    mozlz4.url = "github:jusw85/mozlz4";

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

  outputs = inputs:
    {
      hmModules.firefoxSearchEngines = import ./hm-module;
      nixosConfigurations = import ./nixos inputs;
      nixosModules.base = import ./module;
      overlays.default = import ./overlay inputs;
      templates = import ./templates;
    }
    // inputs.fu.lib.eachSystem [inputs.fu.lib.system.x86_64-linux] (system: let
      pkgs = import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [inputs.self.overlays.default];
      };
    in {
      apps = import ./apps.nix pkgs;
      devShells.default = pkgs.callPackage ./shell.nix {};
      packages = inputs.fu.lib.flattenTree (import ./packages.nix pkgs);
    });
}
