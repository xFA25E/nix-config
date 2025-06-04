{
  config,
  inputs,
  lib,
  modulesPath,
  pkgs,
  username,
  ...
}: {
  imports = [(modulesPath + "/installer/scan/not-detected.nix")];

  boot.tmp.cleanOnBoot = true;

  console = {
    font = "Lat2-Terminus16";
    keyMap = "dvorak";
  };

  documentation.man.generateCaches = true;

  fonts = {
    enableGhostscriptFonts = true;
    fontDir.enable = true;
    fontconfig.defaultFonts.monospace = ["Iosevka"];
    packages = with pkgs; [
      corefonts
      fira-code
      font-awesome
      hack-font
      hasklig
      inconsolata
      iosevka
      # nerdfonts
      open-sans
      source-code-pro
      unifont
    ];
  };

  i18n.defaultLocale = "en_US.UTF-8";

  networking = {
    firewall.allowedTCPPorts = [8080 8000];
    hosts = {
      "0.0.0.0" = [
        "api.rewards.brave.com"
        "brave-core-ext.s3.brave.com"
        "grant.rewards.brave.com"
        "laptop-updates.brave.com"
        "rewards.brave.com"
        "static1.brave.com"
        "variations.brave.com"
      ];
    };
    stevenblack = {
      enable = true;
      block = ["gambling"];
    };
  };

  nix = {
    gc = {
      automatic = true;
      dates = "weekly";
      options = "--delete-older-than 30d";
    };
    registry = {
      nix-config.flake = inputs.self;
      nixpkgs.flake = inputs.nixpkgs;
    };
    package = pkgs.nix;
    settings = {
      auto-optimise-store = true;
      bash-prompt-suffix = ''$(printf '\10\10')nix \$ $(:)'';
      experimental-features = ["nix-command" "flakes"];
      keep-derivations = true;
      keep-outputs = true;
      max-jobs = "auto";
      nix-path = ["nixpkgs=${inputs.nixpkgs}"];
    };
  };

  programs.bash.promptInit = ''
    PS1='\n$(e=$?;[[ $e != 0 ]]&&printf "%s " "$e")\u $(p=''${PWD#"$HOME"};[[ $PWD != "$p" ]]&&printf "~";IFS=/;for q in ''${p:1};do printf "/%s" "''${q:0:1}";[[ ''${q:0:1} = . ]]&&printf "%s" "''${q:1:1}";done;[[ ''${q:0:1} != . ]]&&printf "%s" "''${q:1:1}";printf "%s" "''${q:2}") \$ '
  '';

  services = {
    locate = {
      enable = true;
      interval = "13:00";
      package = pkgs.plocate;
    };

    nscd.enableNsncd = true;
    udisks2.enable = true;
  };

  systemd.services = {
    "loadkeys" = {
      enable = true;
      description = "Change caps to ctrl";
      wantedBy = ["default.target"];
      unitConfig = {
        Type = "oneshot";
      };
      serviceConfig = {
        ExecStart = "${pkgs.kbd}/bin/loadkeys ${./ctrl2caps.map}";
      };
    };
  };

  time.timeZone = "Europe/Rome";

  users.users.${username} = {
    initialHashedPassword = "";
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };

  programs.nix-ld.enable = true;
}
