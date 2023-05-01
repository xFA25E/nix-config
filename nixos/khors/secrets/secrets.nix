let
  key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPWsXrQIclWkeQ98AzZg98+nt6QvQsTJczUC1KlA0QP0 root@khors";
in {
  "mail.age".publicKeys = [key];
}
