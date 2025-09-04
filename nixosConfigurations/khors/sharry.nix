{
  config,
  inputs,
  pkgs,
  ...
}: let
  port = 57052;
  user = "sharry";
  dbName = "sharry";
  dbPort = config.services.postgresql.settings.port;
in {
  imports = [inputs.sharry.nixosModules.default];

  networking.firewall.allowedTCPPorts = [443 80];

  services = {
    nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedOptimisation = true;
      recommendedTlsSettings = true;
      recommendedProxySettings = true;
      virtualHosts."sharry.litkov.one" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          proxyPass = "http://localhost:${toString port}/";
          proxyWebsockets = true;
        };
        extraConfig = ''
          client_max_body_size 4G;
          proxy_request_buffering off;
          proxy_buffering off;
          client_body_timeout 1h;
          proxy_read_timeout  1h;
          proxy_send_timeout  1h;
          add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
        '';
      };
    };

    postgresql = {
      enable = true;
      ensureDatabases = [dbName];
      ensureUsers = [
        {
          name = user;
          ensureDBOwnership = true;
        }
      ];
    };

    sharry = {
      enable = true;
      config = {
        backend = {
          auth.fixed = {
            enabled = true;
            user = "admin";
            password = "";
          };
          files = {
            copy-files = {
              enable = false;
              source = "database";
              target = "filesystem";
            };
            default-store = "filesystem";
            stores = {
              database.enabled = false;
              filesystem = {
                enabled = true;
                directory = "/var/lib/sharry";
              };
            };
          };
          jdbc = {
            inherit user;
            url = "jdbc:postgresql://localhost:${toString dbPort}/${dbName}";
          };
          signup.mode = "closed";
        };
        base-url = "https://sharry.litkov.one";
        bind = {
          inherit port;
          address = "localhost";
        };
      };
    };
  };

  systemd.services = {
    "postgresql-provision-sharry" = {
      description = "Provision sharry DB password";
      after = ["postgresql.service"];
      requires = ["postgresql.service"];
      wantedBy = ["multi-user.target"];

      path = [pkgs.postgresql];

      script = let
        psqlScript = pkgs.writeText "provision-sharry.sql" "ALTER ROLE ${user} WITH LOGIN PASSWORD :'pw';";
      in ''
        psql -v ON_ERROR_STOP=1 -v pw="$SHARRY_BACKEND_JDBC_PASSWORD" -f ${psqlScript}
      '';

      serviceConfig = {
        User = "postgres";
        Group = "postgres";
        Type = "oneshot";
        EnvironmentFile = config.age.secrets."sharry".path;
      };
    };

    "sharry" = {
      after = ["postgresql.service" "postgresql-provision-sharry.service"];
      requires = ["postgresql.service" "postgresql-provision-sharry.service"];
      serviceConfig = {
        EnvironmentFile = config.age.secrets."sharry".path;

        StateDirectory = "sharry";
        ReadWritePaths = "/var/lib/sharry";
        StateDirectoryMode = "0700";
        UMask = "0077";

        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        ProtectClock = true;
        ProtectHostname = true;
        LockPersonality = true;
        NoNewPrivileges = true;
        RestrictSUIDSGID = true;
        RestrictRealtime = true;
        SystemCallArchitectures = "native";

        IPAddressDeny = "any";
        IPAddressAllow = ["127.0.0.1" "::1"];
        RestrictAddressFamilies = ["AF_INET" "AF_INET6" "AF_UNIX"];

        CapabilityBoundingSet = "";
        AmbientCapabilities = "";

        # risky
        ProcSubset = "pid";
        ProtectProc = "invisible";
        RestrictNamespaces = true;
        PrivateMounts = true;
        RemoveIPC = true;
      };
    };
  };
}
