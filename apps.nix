pkgs: {
  default = {
    type = "app";
    program = "${pkgs.preparehd}/bin/preparehd";
  };
  install_keys = {
    type = "app";
    program = "${pkgs.install_keys}/bin/install_keys";
  };
  switch = {
    type = "app";
    program = toString (pkgs.writeShellScript "switch" ''
      sudo -A nixos-rebuild switch --print-build-logs --flake .
    '');
  };
}
