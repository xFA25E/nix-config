{
  inputs,
  username,
  ...
}: {
  boot.cleanTmpDir = true;

  i18n.defaultLocale = "en_US.UTF-8";

  networking.hostName = "khors";

  nix = {
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

  # security.acme = {
  #   acceptTerms = true;
  #   defaults.email = "valeriy@litkov.one";
  # };

  services.openssh = {
    enable = true;
    permitRootLogin = "no";
    passwordAuthentication = false;
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
