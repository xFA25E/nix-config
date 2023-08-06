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
      sudo -A nixos-rebuild switch --print-build-logs --flake ~/Documents/projects/nix-config
    '');
  };
  deployPerun = {
    type = "app";
    program = toString (pkgs.writeShellScript "deployPerun" ''
      export NIX_SSHOPTS=-t
      nixos-rebuild -L switch --flake .#perun --target-host perun \
                              --use-remote-sudo --print-build-logs
    '');
  };
  deployKhors = {
    type = "app";
    program = toString (pkgs.writeShellScript "deployKhors" ''
      export NIX_SSHOPTS=-t
      nixos-rebuild -L switch --flake .#khors --target-host khors \
                              --use-remote-sudo --print-build-logs
    '');
  };
}
