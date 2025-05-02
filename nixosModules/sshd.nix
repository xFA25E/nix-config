{username, ...}: {
  nix.settings.trusted-users = [username];

  services.openssh = {
    enable = true;
    settings.PermitRootLogin = "no";
    settings.PasswordAuthentication = false;
  };

  users.users.${username} = {
    openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDEbZ9Kz4oCbrybWc7jM6Oc7+UKFDsXtb/8IzmtpRb5flqXKy0ghUDLAQl/tur7du0HuX8la5Qsko/IbXN2ZK+2lqiWUnszAPA8P6DdLLO+U9W6yR5LqpIZLpDOwhQVf/IkrNEQXAGEP46YpYLLsn6SATQnXSy87Ri/au6+4joOMoQN9rjKPDD638BDDzFMf3fEbDotC1H5sBPHlrk09hsD4/pyrxmn7UJouT6cGWkuqXAx/NclGnay9hAsue00QqUYK62IC2wE9vNvmzWlAX2eVzo4envypNTe4tYDYS6BGklZP04AcgOwlVeOO+eUkPFMLgVSfJyNqpxXIugS05w9 vlr.ltkvsk@protonmail.com"
    ];
  };
}
