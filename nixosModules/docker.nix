{username, ...}: {
  users.users.${username}.extraGroups = ["docker"];
  virtualisation.docker = {
    enable = true;
    storageDriver = "btrfs";
  };
}
