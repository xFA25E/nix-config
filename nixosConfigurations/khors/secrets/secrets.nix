let
  key = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOtHgputLmtvS/aOpUnPeh92DaeA4/j5LyrGTttED8OA root@ssdnodes-6454ad7db3ff9";
in {
  "mail.age".publicKeys = [key];
}
