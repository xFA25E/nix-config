{
  config,
  lib,
  pkgs,
  ...
}: let
  borgPull = pkgs.writeShellApplication {
    name = "borg-pull";
    runtimeInputs = [pkgs.borgbackup pkgs.socat pkgs.coreutils pkgs.util-linux];
    text = ''
      # Passphrase arrives on stdin from the desktop and is never stored here
      IFS= read -r BORG_PASSPHRASE
      export BORG_PASSPHRASE
      export BORG_RSH="sh -c 'exec socat STDIO UNIX-CONNECT:/run/borg/repo.sock'"

      # Host is ignored; the path must match --restrict-to-repository on the desktop
      exec nice -n 19 ionice -c2 -n7 borg create --stats --compression auto,zstd --exclude-caches --lock-wait 3600 \
        'ssh://borg@desktop/mnt/backup-local/khors-repo::khors-{now}' \
        /var/lib/immich/backups \
        /var/lib/immich/library \
        /var/lib/immich/profile \
        /var/lib/immich/upload \
        /var/lib/immich-external-libraries \
        /var/vmail \
        /var/dkim \
        /var/lib/redis-rspamd \
        /var/lib/acme
    '';
  };
in {
  users.groups.backup-trigger = {};
  users.users.backup-trigger = {
    isSystemUser = true;
    group = "backup-trigger";
    useDefaultShell = true;
    openssh.authorizedKeys.keys = [
      ''restrict,port-forwarding,command="/run/wrappers/bin/sudo ${borgPull}/bin/borg-pull" ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIOHTAY6ILUi3L5ECwgbUHDU5KERMX4R1dCVRaYoClXzQ backup-trigger''
    ];
  };

  security.sudo.extraRules = [
    {
      users = ["backup-trigger"];
      commands = [
        {
          command = "${borgPull}/bin/borg-pull";
          options = ["NOPASSWD"];
        }
      ];
    }
  ];

  # The reverse-forwarded socket is created by this sshd, so this must be set here
  services.openssh.extraConfig = "StreamLocalBindUnlink yes";

  systemd.tmpfiles.rules = ["d /run/borg 0700 backup-trigger backup-trigger -"];
}
