{
  base = import ./base.nix;
  bluetooth = import ./bluetooth.nix;
  docker = import ./docker.nix;
  home = import ./home.nix;
  nvidia = import ./nvidia.nix;
  pipewire = import ./pipewire.nix;
  sshd = import ./sshd.nix;
  x = import ./x.nix;
  zsa = import ./zsa.nix;
}
