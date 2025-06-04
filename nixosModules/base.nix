{
  inputs,
  pkgs,
  username,
  ...
}: {
  boot.tmp.cleanOnBoot = true;
  i18n.defaultLocale = "en_US.UTF-8";

  networking = {
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

  time.timeZone = "Europe/Rome";

  users.users.${username} = {
    initialHashedPassword = "";
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };
}
