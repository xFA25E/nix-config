{
  base = import ./base.nix;
  home = import ./home.nix;
  nvidia = import ./nvidia.nix;
  sshd = import ./sshd.nix;
  x = import ./x.nix;
  zsa = import ./zsa.nix;
}
