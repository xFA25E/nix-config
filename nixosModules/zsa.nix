{
  pkgs,
  username,
  ...
}: {
  hardware.keyboard.zsa.enable = true;
  users.users.${username}.packages = [pkgs.wally-cli];
}
