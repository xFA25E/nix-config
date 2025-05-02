{
  description = "xFA25E various nix configurations";

  inputs = {
    addictions-tracker.inputs.flake-utils.follows = "fu";
    addictions-tracker.inputs.nixpkgs.follows = "nixpkgs";
    addictions-tracker.url = "github:xFA25E/AddictionsTracker";

    agenix.inputs.nixpkgs.follows = "nixpkgs";
    agenix.url = "github:ryantm/agenix";

    discord.flake = false;
    discord.url = "https://dl.discordapp.net/apps/linux/0.0.90/discord-0.0.90.tar.gz";

    # emacs-overlay.inputs.flake-utils.follows = "fu";
    emacs-overlay.inputs.nixpkgs-stable.follows = "nixpkgs";
    emacs-overlay.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    epkg-abbrev-hook.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-abbrev-hook.inputs.nixpkgs.follows = "nixpkgs";
    epkg-abbrev-hook.url = "github:xFA25E/abbrev-hook";

    epkg-amded.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-amded.inputs.nixpkgs.follows = "nixpkgs";
    epkg-amded.url = "github:xFA25E/amded";

    epkg-cyrillic-dvorak-im.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-cyrillic-dvorak-im.inputs.nixpkgs.follows = "nixpkgs";
    epkg-cyrillic-dvorak-im.url = "github:xFA25E/cyrillic-dvorak-im";

    epkg-dired-atool-transient.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-dired-atool-transient.inputs.nixpkgs.follows = "nixpkgs";
    epkg-dired-atool-transient.url = "github:xFA25E/dired-atool-transient";

    epkg-dired-tags.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-dired-tags.inputs.epkg-xattr.follows = "epkg-xattr";
    epkg-dired-tags.inputs.nixpkgs.follows = "nixpkgs";
    epkg-dired-tags.url = "github:xFA25E/dired-tags";

    epkg-enwc-iwd.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-enwc-iwd.inputs.nixpkgs.follows = "nixpkgs";
    epkg-enwc-iwd.url = "github:xFA25E/enwc-iwd";

    epkg-pueue.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-pueue.inputs.nixpkgs.follows = "nixpkgs";
    epkg-pueue.url = "github:xFA25E/pueue";

    epkg-rx-widget.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-rx-widget.inputs.nixpkgs.follows = "nixpkgs";
    epkg-rx-widget.url = "github:xFA25E/rx-widget";

    epkg-sdcwoc.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-sdcwoc.inputs.nixpkgs.follows = "nixpkgs";
    epkg-sdcwoc.url = "github:xFA25E/sdcwoc";

    epkg-tempo-extra.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-tempo-extra.inputs.epkg-abbrev-hook.follows = "epkg-abbrev-hook";
    epkg-tempo-extra.inputs.nixpkgs.follows = "nixpkgs";
    epkg-tempo-extra.url = "github:xFA25E/tempo-extra";

    epkg-xattr.inputs.emacs-overlay.follows = "emacs-overlay";
    epkg-xattr.inputs.nixpkgs.follows = "nixpkgs";
    epkg-xattr.url = "github:xFA25E/xattr";

    fu.url = "github:numtide/flake-utils";

    grobi.flake = false;
    grobi.url = "github:fd0/grobi";

    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    home-manager.url = "github:nix-community/home-manager/release-24.11";

    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";

    nix-colors.url = "github:Misterio77/nix-colors";

    nur.url = "github:nix-community/NUR";

    simple-nixos-mailserver.inputs.nixpkgs.follows = "nixpkgs";
    simple-nixos-mailserver.inputs.nixpkgs-24_11.follows = "nixpkgs";
    simple-nixos-mailserver.url = "gitlab:simple-nixos-mailserver/nixos-mailserver/nixos-24.11";

    stumpwm.flake = false;
    stumpwm.url = "github:stumpwm/stumpwm/24.11";

    # lem-flake.inputs.nixpkgs.follows = "nixpkgs";
    # lem-flake.url = "github:dariof4/lem-flake";
  };

  outputs = inputs:
    {
      nixosConfigurations = import ./nixosConfigurations inputs;
      nixosModules = import ./nixosModules;
      homeModules = import ./homeModules;
      overlays = import ./overlays inputs;
      templates = import ./templates;
    }
    // inputs.fu.lib.eachSystem [inputs.fu.lib.system.x86_64-linux] (system: let
      pkgs = import inputs.nixpkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = [inputs.agenix.overlays.default inputs.self.overlays.default];
      };
    in {
      apps = import ./apps.nix pkgs;
      devShells.default = pkgs.callPackage ./shell.nix {};
      packages = inputs.fu.lib.flattenTree (import ./packages.nix pkgs);
    });
}
