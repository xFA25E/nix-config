{
  base = import ./base.nix;
  bluetooth = import ./bluetooth.nix;
  desktop = import ./desktop.nix;
  docker = import ./docker.nix;
  home = import ./home.nix;
  nvidia = import ./nvidia.nix;
  ollama = import ./ollama.nix;
  pipewire = import ./pipewire.nix;
  podman = import ./podman.nix;
  sshd = import ./sshd.nix;
  x = import ./x.nix;
  zsa = import ./zsa.nix;
}
