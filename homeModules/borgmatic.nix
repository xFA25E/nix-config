{
  config,
  lib,
  osConfig ? null,
  pkgs,
  ...
}: let
  isSvarog = osConfig != null && osConfig.networking.hostName == "svarog";
in {
  config = lib.mkIf isSvarog {
    systemd.user = {
      sockets.khors-borg = {
        Unit.Description = "borg serve socket for VPS pull backup";
        Socket = {
          ListenStream = "%t/khors-borg/borg.sock";
          Accept = true;
          SocketMode = "0600";
          DirectoryMode = "0700";
        };
        Install.WantedBy = ["sockets.target"];
      };

      services = {
        "khors-borg@" = {
          Unit.ConditionPathIsMountPoint = "/mnt/backup-local";
          Service = {
            ExecStart = "${pkgs.borgbackup}/bin/borg serve --append-only --restrict-to-repository /mnt/backup-local/khors-repo";
            StandardInput = "socket";
            StandardOutput = "socket";
            StandardError = "journal"; # stderr on the socket breaks the borg protocol
            Nice = 19;
            IOSchedulingClass = "best-effort";
            IOSchedulingPriority = 7;
          };
        };

        # --- trigger: open the tunnel, feed the passphrase, block until done ---
        khors-backup = {
          Unit.ConditionPathIsMountPoint = "/mnt/backup-local";
          Service = {
            Type = "oneshot";
            LogRateLimitIntervalSec = 0;
            ExecStartPre = "${pkgs.coreutils}/bin/sleep 3m";
            ExecStart = let
              khorsBackupScript = pkgs.writeShellScript "khors-backup-script" ''
                set -o pipefail
                ${pkgs.pass}/bin/pass show other/backup-khors | ${pkgs.openssh}/bin/ssh -T \
                  -i "$HOME/.ssh/id_ed25519_backup-trigger" \
                  -o IdentitiesOnly=yes \
                  -o BatchMode=yes \
                  -o ExitOnForwardFailure=yes \
                  -o ServerAliveInterval=30 \
                  -o ServerAliveCountMax=6 \
                  -R /run/borg/repo.sock:"$XDG_RUNTIME_DIR/khors-borg/borg.sock" \
                  backup-trigger@khors
              '';
            in ''
              ${pkgs.systemd}/bin/systemd-inhibit \
                --who="khors-backup" \
                --what="sleep:shutdown" \
                --why="Prevent interrupting pull backup" \
                "${khorsBackupScript}"
            '';
          };
        };

        khors-repo-maintenance = {
          Unit = {
            ConditionPathIsMountPoint = "/mnt/backup-local";
            After = ["khors-backup.service"];
          };
          Service = {
            Type = "oneshot";
            Environment = ["PATH=${lib.makeBinPath [pkgs.borgbackup pkgs.pass pkgs.coreutils]}"];
            ExecStartPre = "${pkgs.coreutils}/bin/sleep 3m";
            ExecStart = let
              khorsMaintenanceYaml = pkgs.writeText "khors-maintenance.yaml" (lib.generators.toYAML {} {
                repositories = [
                  {
                    label = "khors";
                    path = "/mnt/backup-local/khors-repo";
                  }
                ];
                encryption_passcommand = "${pkgs.pass}/bin/pass show other/backup-khors";
                keep_daily = 7;
                keep_weekly = 4;
                keep_monthly = 12;
                checks = [
                  {
                    name = "repository";
                    frequency = "2 weeks";
                  }
                  {
                    name = "archives";
                    frequency = "4 weeks";
                  }
                  {
                    name = "data";
                    frequency = "6 weeks";
                  }
                  {
                    name = "extract";
                    frequency = "6 weeks";
                  }
                ];
                lock_wait = 3600;
              });
            in ''
              ${pkgs.systemd}/bin/systemd-inhibit \
                --who="khors-repo-maintenance" \
                --what="sleep:shutdown" \
                --why="Prevent interrupting khors repo maintenance" \
                ${pkgs.borgmatic}/bin/borgmatic \
                  -c ${khorsMaintenanceYaml} prune compact check --verbosity -1 --syslog-verbosity 1
            '';
            Nice = 19;
            IOSchedulingClass = "best-effort";
            IOSchedulingPriority = 7;
            LogRateLimitIntervalSec = 0;
          };
        };
      };

      timers = {
        khors-backup = {
          Timer = {
            OnCalendar = "daily";
            Persistent = true;
            RandomizedDelaySec = "10m";
          };
          Install.WantedBy = ["timers.target"];
        };

        khors-repo-maintenance = {
          Timer = {
            OnCalendar = "weekly";
            Persistent = true;
            RandomizedDelaySec = "10m";
          };
          Install.WantedBy = ["timers.target"];
        };
      };
    };

    programs.borgmatic = {
      enable = true;
      backups = {
        svarog = {
          consistency.checks = [
            {
              name = "repository";
              frequency = "2 weeks";
            }
            {
              name = "archives";
              frequency = "4 weeks";
            }
            {
              name = "data";
              frequency = "6 weeks";
            }
            {
              name = "extract";
              frequency = "6 weeks";
            }
          ];

          location = {
            excludeHomeManagerSymlinks = true;
            repositories = [
              {
                label = "svarog";
                path = "/mnt/backup-local/svarog-repo";
              }
            ];

            sourceDirectories = [
              "~/Documents"
              "~/Downloads"
              "~/Music"
              "~/Pictures"
              "~/Videos"
              "~/archives"
              "~/org"
              "~/roam"
              "~/phone"
              "~/.authinfo.gpg"
              "~/.bash_history"
              "~/.gnupg"
              "~/.ssh"
              "~/.local/share/emacs"
              "~/.local/share/mail"
              "~/.local/share/password-store"
              "~/.config/transmission-daemon"
            ];
          };

          retention = {
            keepDaily = 7;
            keepWeekly = 4;
            keepMonthly = 12;
          };

          storage = {
            encryptionPasscommand = "${pkgs.pass}/bin/pass show other/backup-local";
          };
        };
      };
    };

    services.borgmatic = {
      enable = true;
      frequency = "daily";
    };
  };
}
