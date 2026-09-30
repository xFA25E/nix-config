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
      substituters = ["https://cache.nixos-cuda.org"];
      trusted-public-keys = ["cache.nixos-cuda.org:74DUi4Ye579gUqzH4ziL9IyiJBlDpMRn9MBN8oNan9M="];
    };
  };

  programs.bash.promptInit = ''
    # 1. DEBUG trap captures start time of actual typed commands
    trap 'timer=''${timer:-$SECONDS}' DEBUG

    build_prompt() {
      # CRITICAL: Capture exit code of user's command immediately
      local exit_code=$?

      # --- A. Timestamp & Duration Logic ---
      if [ -n "$timer" ]; then
        local elapsed=$((SECONDS - timer))
        local time_part="$(date +'%Y-%m-%d %H:%M:%S') ''${elapsed}s "
        unset timer
      fi

      # --- B. Error Status ---
      local err_str=""
      if [[ $exit_code != 0 ]]; then
        err_str="$exit_code "
      fi

      # --- C. Shortened Path Logic ---
      local path=''${PWD#"$HOME"}
      local path_str=""

      if [[ $PWD != "$path" ]]; then
        path_str="~"
      fi

      local IFS=/
      local part
      for part in ''${path:1}; do
        path_str="$path_str/''${part:0:1}"
        if [[ ''${part:0:1} = . ]]; then
          path_str="$path_str''${part:1:1}"
        fi
      done

      if [[ -n "$part" ]]; then
        if [[ ''${part:0:1} != . ]]; then
          path_str="$path_str''${part:1:1}"
        fi
        path_str="$path_str''${part:2}"
      fi

      # --- D. Assemble PS1 directly ---
      PS1="\n''${time_part}\n''${err_str}\u ''${path_str} \$ "
    }

    PROMPT_COMMAND=build_prompt
  '';

  time.timeZone = "Europe/Rome";

  users.users.${username} = {
    initialHashedPassword = "";
    isNormalUser = true;
    extraGroups = ["wheel" "networkmanager"];
  };
}
